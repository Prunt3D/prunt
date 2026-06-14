import os
import sys
import glob
import subprocess
import re
import json
import shutil
import concurrent.futures

OUTPUT_DIR = "gnatcov_out"
REPORT_DIR = os.path.join(OUTPUT_DIR, "report")
GNATCOV = os.environ.get("GNATCOV") or shutil.which("gnatcov") or "gnatcov"

def ensure_dir(path):
    if not os.path.exists(path):
        os.makedirs(path)

def run_gnatcov_for_trace(payload):
    i, trace, extra_args = payload
    trace_name = os.path.basename(trace)

    trace_out_dir = os.path.join(OUTPUT_DIR, f"trace_{i}")
    ensure_dir(trace_out_dir)

    cmd = [
        GNATCOV,
        "coverage"
    ] + extra_args + [
        "--annotate=xcov+",
        "--level=stmt+mcdc+gexpr",
        "--projects=prunt.gpr",
        "--no-subprojects",
        f"--output-dir={trace_out_dir}",
        trace
    ]

    try:
        subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        return (i, trace_out_dir, True)
    except subprocess.CalledProcessError as e:
        print(f"Error running gnatcov for {trace_name}: {e}")
        return (i, trace_out_dir, False)

def parse_xcov_files(coverage_data, test_id, source_dir, is_global=False):
    xcov_files = glob.glob(os.path.join(source_dir, "*.xcov"))

    line_pattern = re.compile(r"^\s*(\d+)\s([+\-!.*?]):(.*)")

    # helper for message pool
    def get_msg_id(msg):
        if msg not in coverage_data["msg_map"]:
            idx = len(coverage_data["msg_pool"])
            coverage_data["msg_pool"].append(msg)
            coverage_data["msg_map"][msg] = idx
        return coverage_data["msg_map"][msg]

    for xcov_path in xcov_files:
        filename = os.path.basename(xcov_path).replace(".xcov", "")
        # Assuming filename maps to source file. prunt-config.adb.xcov -> prunt-config.adb

        if filename not in coverage_data["files"]:
            coverage_data["files"][filename] = {
                "source": [],
                "lines": {}
            }

        with open(xcov_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()

        current_source_lines = []
        last_line_num = -1
        last_line_status = None

        for line in lines:
            match = line_pattern.match(line)
            if match:
                line_num = int(match.group(1))
                status = match.group(2)
                content = match.group(3)

                if not coverage_data["files"][filename]["source"]:
                    current_source_lines.append(content)

                last_line_num = line_num
                last_line_status = status

                if status in ['+', '!', '-', '*', '?']:
                    if str(line_num) not in coverage_data["files"][filename]["lines"]:
                        coverage_data["files"][filename]["lines"][str(line_num)] = {
                            "mask_full": 0,
                            "mask_partial": 0,
                            "mask_unknown": 0,
                            "exempt": (status == '*'),
                            "msgs": {}
                        }

                    obj = coverage_data["files"][filename]["lines"][str(line_num)]

                    if is_global:
                         if "global" not in obj:
                             obj["global"] = {}
                         obj["global"]["status"] = status
                         obj["global"]["msgs"] = []
                    else:
                        if status == '+':
                            obj["mask_full"] |= (1 << test_id)
                        elif status == '!':
                            obj["mask_partial"] |= (1 << test_id)
                        elif status == '?':
                            obj["mask_unknown"] |= (1 << test_id)

                        if status == '*':
                            obj["exempt"] = True

            else:
                if last_line_num != -1 and line.strip() and not line.startswith("Coverage level") and not line.startswith("/"):
                    if last_line_status == '!':
                        line_str = str(last_line_num)
                        if line_str in coverage_data["files"][filename]["lines"]:
                            obj = coverage_data["files"][filename]["lines"][line_str]

                            msg_id = get_msg_id(line.strip())

                            if is_global:
                                if "global" in obj:
                                    obj["global"]["msgs"].append(msg_id)
                            else:
                                if test_id not in obj["msgs"]:
                                    obj["msgs"][test_id] = []
                                obj["msgs"][test_id].append(msg_id)

        if not coverage_data["files"][filename]["source"] and current_source_lines:
             coverage_data["files"][filename]["source"] = current_source_lines

def hex_encode_masks(coverage_data):
    for filename, file_data in coverage_data["files"].items():
        for line_num, line_data in file_data["lines"].items():
            if line_data["mask_full"] != 0:
                line_data["mask_full"] = hex(line_data["mask_full"])[2:]
            else:
                line_data["mask_full"] = "0"

            if line_data["mask_partial"] != 0:
                line_data["mask_partial"] = hex(line_data["mask_partial"])[2:]
            else:
                line_data["mask_partial"] = "0"

            if line_data["mask_unknown"] != 0:
                line_data["mask_unknown"] = hex(line_data["mask_unknown"])[2:]
            else:
                line_data["mask_unknown"] = "0"

def main():
    print("Starting coverage generation...")
    ensure_dir(OUTPUT_DIR)
    ensure_dir(REPORT_DIR)

    traces = glob.glob("*.srctrace")
    traces.sort()

    if not traces:
        print("No .srctrace files found in tests/")
        sys.exit(1)

    print(f"Found {len(traces)} traces. Running gnatcov in parallel...")

    coverage_data = {
        "files": {},
        "tests": [],
        "msg_pool": [],
        "msg_map": {}
    }

    jobs = []
    for i, trace in enumerate(traces):
        jobs.append((i, trace, sys.argv[1:]))

        trace_name = os.path.basename(trace)
        match = re.search(r"individual_test-(.*)-[0-9a-f]+-[0-9a-f]+-[0-9a-f]+-[0-9a-f]+\.srctrace", trace_name)
        test_display_name = match.group(1) if match else trace_name

        coverage_data["tests"].append({
            "id": i,
            "name": test_display_name
        })

    max_workers = os.cpu_count() or 4
    print(f"Using {max_workers} worker threads.")

    results = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=max_workers) as executor:
        future_to_job = {executor.submit(run_gnatcov_for_trace, job): job for job in jobs}

        completed = 0
        for future in concurrent.futures.as_completed(future_to_job):
            completed += 1
            if completed % 10 == 0 or completed == len(jobs):
                print(f"Processed {completed}/{len(jobs)} traces...")

            try:
                result = future.result()
                results.append(result)
            except Exception as exc:
                print(f"Job generated an exception: {exc}")

    print("Parsing individual coverage data...")
    for i, trace_out_dir, success in results:
        if success:
             parse_xcov_files(coverage_data, i, trace_out_dir, is_global=False)
             try:
                shutil.rmtree(trace_out_dir)
             except OSError:
                pass
        else:
            print(f"Skipping parsing for test {i} due to gnatcov failure.")

    print("Running global gnatcov (all traces)...")
    global_out_dir = os.path.join(OUTPUT_DIR, "global_coverage")
    ensure_dir(global_out_dir)

    cmd = [
        GNATCOV,
        "coverage"
    ] + sys.argv[1:] + [
        "--annotate=xcov+",
        "--level=stmt+mcdc+gexpr",
        "--projects=prunt.gpr",
        "--no-subprojects",
        f"--output-dir={global_out_dir}"
    ] + traces

    subprocess.run(cmd, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    print("Global run successful. Parsing global data...")
    parse_xcov_files(coverage_data, -1, global_out_dir, is_global=True)
    try:
        shutil.rmtree(global_out_dir)
    except OSError:
        pass

    del coverage_data["msg_map"]

    print("Encoding data...")
    hex_encode_masks(coverage_data)

    print("Generating HTML...")
    generate_html(coverage_data)

    print(f"Done! Report available at {os.path.join(REPORT_DIR, 'index.html')}")

def generate_html(data):
    json_str = json.dumps(data)

    html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Prunt Coverage Report</title>
    <style>
        body {{ font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; margin: 0; display: flex; height: 100vh; overflow: hidden; }}

        /* Layout & Resizers */
        .pane {{ background: #f5f5f5; overflow-y: auto; position: relative; display: flex; flex-direction: column; min-width: 50px; }}
        .resizer {{ width: 5px; background: #ddd; cursor: col-resize; z-index: 10; }}
        .resizer:hover {{ background: #bbb; }}

        #file-list {{ width: 20%; border-right: 1px solid #ccc; }}
        #source-view {{ flex: 1; overflow-y: auto; padding: 0; position: relative; min-width: 0; }}
        #test-list {{ width: 25%; border-left: 1px solid #ccc; }}

        .collapse-btn {{
            position: absolute; top: 0; width: 30px; height: 30px; background: #eee; border: none; cursor: pointer; z-index: 5;
            display: flex; align-items: center; justify-content: center; font-weight: bold; color: #555;
        }}
        #file-list .collapse-btn {{ right: 5px; }}
        #test-list .collapse-btn {{ left: 5px; }}

        /* In collapsed state just hide content and reduce width.
           We handle this via JS width setting usually, but CSS class helps with content hiding. */
        .pane.collapsed {{ width: 40px !important; }}
        .pane.collapsed > :not(.collapse-btn) {{ display: none !important; }}

        /* File List Items */
        .file-item {{ padding: 10px; cursor: pointer; border-bottom: 1px solid #eee; }}
        .file-item:hover {{ background: #e0e0e0; }}
        .file-item.active {{ background: #d0d0ff; }}
        .file-name {{ font-weight: bold; font-size: 0.9em; }}

        /* Multi-part progress bar */
        .coverage-bar {{ height: 8px; background: #ddd; margin-top: 5px; border-radius: 4px; overflow: hidden; display: flex; }}
        .bar-full {{ background: #4caf50; height: 100%; }}
        .bar-partial {{ background: #ffeb3b; height: 100%; }}
        .bar-none {{ background: #f44336; height: 100%; }}
        .bar-unknown {{ background: #9e9e9e; height: 100%; }}

        .stats-text {{ font-size: 0.75em; text-align: right; margin-top: 2px; color: #555; }}

        /* Source View */
        .code-line {{ font-family: Consolas, monospace; white-space: pre; font-size: 13px; display: flex; }}
        .line-status {{ width: 24px; text-align: center; color: #666; user-select: none; border-right: 1px solid #eee; background: #f9f9f9; flex-shrink: 0; }}
        .line-num {{ width: 40px; text-align: right; padding-right: 10px; color: #888; user-select: none; border-right: 1px solid #ddd; background: #fafafa; flex-shrink: 0; }}
        .line-count {{ width: 30px; text-align: center; color: #007acc; cursor: pointer; user-select: none; border-right: 1px solid #ddd; background: #f0f7ff; font-size: 0.9em; flex-shrink: 0; }}
        .line-count:hover {{ background: #dcefff; text-decoration: underline; }}
        .line-content {{ padding-left: 10px; flex: 1; }}

        .c-full {{ background-color: #e6ffec; }}
        .c-partial {{ background-color: #fffde7; }}
        .c-none {{ background-color: #ffebee; }}
        .c-unknown {{ background-color: #f0f0f0; color: #888; }}
        .c-exempt {{ background-color: #eeeeee; color: #666; }}

        /* Inline Expansion (Messages & Tests) */
        .inline-details {{
            background: #fff; border-bottom: 1px solid #ddd; padding: 5px 10px 5px 100px;
            font-size: 0.85em; display: none; color: #333; box-shadow: inset 0 3px 5px rgba(0,0,0,0.05);
        }}
        .inline-details.visible {{ display: block; }}

        .msg-box {{ padding: 2px 5px; margin-bottom: 2px; font-family: monospace; }}
        .test-box {{ padding: 2px 5px; margin-bottom: 2px; font-family: monospace; color: #555; }}

        .code-line.has-messages {{ cursor: pointer; }}
        .code-line.has-messages .line-status {{ font-weight: bold; color: #d00; }}

        .unique-highlight .line-content {{ background: #e8f5e9; box-shadow: inset 3px 0 0 #4caf50; }}

        @keyframes flash-animation {{
            0% {{ background-color: #ffff00; }}
            100% {{ background-color: transparent; }}
        }}
        .highlight-flash {{ animation: flash-animation 2s ease-out; }}

        /* Unique Lines List */
        .unique-list {{ display: none; margin-left: 20px; border-left: 2px solid #ddd; padding-left: 5px; }}
        .unique-list.visible {{ display: block; }}
        .unique-item {{
            font-size: 0.85em; color: #555; cursor: pointer; padding: 2px 0;
            white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
        }}
        .unique-item:hover {{ color: #2196F3; text-decoration: underline; }}

        /* Tree View */
        ul.tree {{ list-style-type: none; padding-left: 20px; margin: 0; }}
        ul.tree li {{ margin: 2px 0; }}
        .caret {{ cursor: pointer; user-select: none; margin-right: 5px; display: inline-block; width: 10px; }}
        .caret::before {{ content: "\\25B6"; color: black; display: inline-block; font-size: 10px; }}
        .caret-down::before {{ transform: rotate(90deg); }}
        .nested {{ display: none; }}
        .active {{ display: block; }}

        .test-checkbox {{ margin-right: 5px; }}
        .unique-badge {{ margin-left: 5px; font-size: 0.8em; padding: 1px 4px; border-radius: 4px; background: #eee; color: #555; }}
        .unique-badge.redundant {{ background: #ffebee; color: #d32f2f; border: 1px solid #ffcdd2; }}

        /* Global Mode Indicator */
        #global-status {{ background: #2196F3; color: white; padding: 10px; text-align: center; font-weight: bold; display: none; position: sticky; top: 0; z-index: 100; opacity: 0.95; }}

        /* Controls */
        #controls {{ padding: 10px; border-bottom: 1px solid #ddd; background: #fff; }}

    </style>
</head>
<body>
    <!-- File List Pane -->
    <div id="file-list" class="pane">
        <button class="collapse-btn" onclick="togglePane('file-list')">&lt;&lt;</button>
        <div id="file-list-content" style="padding-top: 35px;"></div>
    </div>

    <div class="resizer" id="resizer-1"></div>

    <!-- Source View Pane -->
    <div id="source-view">
        <div id="global-status">GLOBAL COVERAGE MODE (ALL TESTS SELECTED)</div>
        <div id="source-content" style="padding: 10px; min-width: 100%; width: max-content; box-sizing: border-box;">Select a file to view coverage.</div>
    </div>

    <div class="resizer" id="resizer-2"></div>

    <!-- Test List Pane -->
    <div id="test-list" class="pane">
        <button class="collapse-btn" onclick="togglePane('test-list')">&gt;&gt;</button>
        <div style="padding-top: 35px;">
            <div id="controls">
                 <div style="margin-bottom: 5px;">
                    <button onclick="toggleAll(true)">Select All</button>
                    <button onclick="toggleAll(false)">Select None</button>
                </div>
                <div>
                    <label style="font-size: 0.9em; display: flex; align-items: center;">
                        <input type="checkbox" id="intersect-messages-cb" checked onchange="toggleIntersectionMode()">
                        Use Message Intersection
                    </label>
                </div>
            </div>
            <div id="test-tree"></div>
        </div>
    </div>

    <script>
        const data = {json_str};
        let activeFile = null;
        let enabledTests = new Set();
        let fullCoverageMask = 0n;
        let showingAllTests = false;
        let useIntersectionMock = true;
        let uniqueCoverageLocs = {{}}; // Map test_id -> [{{file, line}}]

        // --- Resizable Panes Logic ---
        function initResizers() {{
            const resizer1 = document.getElementById('resizer-1');
            const resizer2 = document.getElementById('resizer-2');
            const filePane = document.getElementById('file-list');
            const testPane = document.getElementById('test-list');
            const sourceView = document.getElementById('source-view');

            makeResizable(resizer1, filePane, true);
            makeResizable(resizer2, testPane, false);
        }}

        function makeResizable(resizer, pane, isLeft) {{
            let startX, startWidth;

            resizer.addEventListener('mousedown', (e) => {{
                startX = e.clientX;
                startWidth = parseInt(document.defaultView.getComputedStyle(pane).width, 10);
                document.documentElement.addEventListener('mousemove', doDrag, false);
                document.documentElement.addEventListener('mouseup', stopDrag, false);
            }});

            function doDrag(e) {{
                let newWidth;
                if (isLeft) {{
                    newWidth = startWidth + e.clientX - startX;
                }} else {{
                    newWidth = startWidth - (e.clientX - startX);
                }}
                pane.style.width = newWidth + 'px';
            }}

            function stopDrag(e) {{
                document.documentElement.removeEventListener('mousemove', doDrag, false);
                document.documentElement.removeEventListener('mouseup', stopDrag, false);
            }}
        }}

        function init() {{
            data.tests.forEach(t => enabledTests.add(t.id));
            toggleAll(true);

            document.getElementById('intersect-messages-cb').checked = useIntersectionMock;
            calculateUniqueStats();
            initResizers();

            renderFileList();
            renderTestTree();
        }}

        function togglePane(id) {{
            const pane = document.getElementById(id);
            const btn = pane.querySelector('.collapse-btn');
            const isLeft = (id === 'file-list');

            pane.classList.toggle('collapsed');

            // Adjust button text (approximate logic)
            if (pane.classList.contains('collapsed')) {{
                btn.innerHTML = isLeft ? '&gt;&gt;' : '&lt;&lt;';
                pane.dataset.oldWidth = pane.style.width || "";
            }} else {{
                btn.innerHTML = isLeft ? '&lt;&lt;' : '&gt;&gt;';
            }}
        }}

        function toggleIntersectionMode() {{
            useIntersectionMock = document.getElementById('intersect-messages-cb').checked;
            renderFileList();
            if (activeFile) renderSource(activeFile);
        }}

        function calculateUniqueStats() {{
            data.tests.forEach(t => uniqueCoverageLocs[t.id] = []);
            Object.keys(data.files).forEach(filename => {{
                const file = data.files[filename];
                Object.keys(file.lines).forEach(lineNum => {{
                    const line = file.lines[lineNum];
                    if (line.exempt) return;
                    const mask = BigInt("0x" + line.mask_full) | BigInt("0x" + line.mask_partial);
                    if (mask === 0n) return;
                    if ((mask & (mask - 1n)) === 0n) {{
                        let temp = mask;
                        let id = 0;
                        while ((temp & 1n) === 0n) {{
                            temp >>= 1n;
                            id++;
                        }}
                        if (uniqueCoverageLocs[id] !== undefined) {{
                            uniqueCoverageLocs[id].push({{file: filename, line: lineNum}});
                        }}
                    }}
                }});
            }});
        }}

        function updateFullMask() {{
             fullCoverageMask = 0n;
             enabledTests.forEach(id => {{
                 fullCoverageMask |= (1n << BigInt(id));
             }});
             showingAllTests = (enabledTests.size === data.tests.length);
             const banner = document.getElementById('global-status');
             banner.style.display = showingAllTests ? 'block' : 'none';
             renderFileList();
             if (activeFile) renderSource(activeFile);
        }}

        function getLineStatus(lineData) {{
            if (lineData.exempt) return {{ status: 'exempt' }};
            if (showingAllTests && lineData.global) {{
                 const status = lineData.global.status;
                 if (status === '+') return {{ status: 'full' }};
                 if (status === '!') return {{ status: 'partial', messages: lineData.global.msgs || [] }};
                 if (status === '?') return {{ status: 'unknown' }};
                 if (status === '-') return {{ status: 'none' }};
            }}
            const maskFull = BigInt("0x" + lineData.mask_full);
            const maskPartial = BigInt("0x" + lineData.mask_partial);
            const maskUnknown = lineData.mask_unknown ? BigInt("0x" + lineData.mask_unknown) : 0n;

            if ((maskFull & fullCoverageMask) !== 0n) return {{ status: 'full' }};

            const activePartialTests = [];
            for (let id of enabledTests) {{
                if ((maskPartial & (1n << BigInt(id))) !== 0n) activePartialTests.push(id);
            }}

            if (activePartialTests.length > 0) {{
                if (!lineData.msgs) return {{ status: 'partial', messages: [] }};
                let resultingMessages = [];
                if (useIntersectionMock) {{
                    let intersection = null;
                    for (let tid of activePartialTests) {{
                        const msgs = lineData.msgs[tid];
                        if (!msgs) continue;
                        if (intersection === null) intersection = new Set(msgs);
                        else {{
                            const nextSet = new Set(msgs);
                            intersection = new Set([...intersection].filter(x => nextSet.has(x)));
                        }}
                        if (intersection.size === 0) break;
                    }}
                    if (intersection && intersection.size === 0) return {{ status: 'full' }};
                    resultingMessages = intersection ? [...intersection] : [];
                }} else {{
                    let union = new Set();
                    for (let tid of activePartialTests) {{
                         const msgs = lineData.msgs[tid];
                         if (msgs) msgs.forEach(m => union.add(m));
                    }}
                    resultingMessages = [...union];
                }}
                return {{ status: 'partial', messages: resultingMessages }};
            }}
            if ((maskUnknown & fullCoverageMask) !== 0n) return {{ status: 'unknown' }};
            return {{ status: 'none' }};
        }}

        function getCoverageStats(filename) {{
            const file = data.files[filename];
            let total = 0, full = 0, partial = 0, none = 0, unknown = 0, exempt = 0;
            for (const lineNum in file.lines) {{
                const line = file.lines[lineNum];
                if (line.exempt) {{ exempt++; continue; }}
                total++;
                const result = getLineStatus(line);
                if (result.status === 'full') full++;
                else if (result.status === 'partial') partial++;
                else if (result.status === 'unknown') unknown++;
                else none++;
            }}
            return {{ total, full, partial, none, unknown, exempt }};
        }}


        function renderFileList() {{
            const container = document.getElementById('file-list-content') || document.getElementById('file-list');
            container.innerHTML = "";
            Object.keys(data.files).sort().forEach(filename => {{
                const stats = getCoverageStats(filename);
                const totalVisible = stats.total;
                const pctFull = totalVisible === 0 ? 0 : (stats.full / totalVisible * 100);
                const pctPartial = totalVisible === 0 ? 0 : (stats.partial / totalVisible * 100);
                const pctUnknown = totalVisible === 0 ? 0 : (stats.unknown / totalVisible * 100);
                const pctNone = totalVisible === 0 ? 0 : (stats.none / totalVisible * 100);

                const div = document.createElement('div');
                div.className = 'file-item' + (activeFile === filename ? ' active' : '');
                div.innerHTML = `
                    <div class="file-name">${{filename}}</div>
                    <div class="coverage-bar">
                        <div class="bar-full" style="width: ${{pctFull}}%"></div>
                        <div class="bar-partial" style="width: ${{pctPartial}}%"></div>
                        <div class="bar-none" style="width: ${{pctNone}}%"></div>
                        <div class="bar-unknown" style="width: ${{pctUnknown}}%"></div>
                    </div>
                    <div class="stats-text">
                       F:${{stats.full}} P:${{stats.partial}} N:${{stats.none}} ?: ${{stats.unknown}} *:${{stats.exempt}}
                    </div>
                `;
                div.onclick = () => {{
                    activeFile = filename;
                    renderFileList();
                    renderSource(filename);
                }};
                container.appendChild(div);
            }});
        }}

        const testTree = {{}};
        function buildTestTree() {{
            data.tests.forEach(test => {{
                const parts = test.name.split('.');
                let current = testTree;
                parts.forEach((part, index) => {{
                    if (!current[part]) current[part] = {{ children: {{}}, tests: [] }};
                    if (index === parts.length - 1) current[part].tests.push(test);
                    current = current[part].children;
                }});
            }});
        }}

        function renderTestTree() {{
            const container = document.getElementById('test-tree');
            container.innerHTML = "";
            if (Object.keys(testTree).length === 0) buildTestTree();
            container.appendChild(createTreeInfo(testTree));
        }}

        function createTreeInfo(node, prefix = "") {{
            const ul = document.createElement('ul');
            ul.className = 'tree';
            if (prefix === "") ul.classList.add('active');
            else ul.classList.add('nested');

            Object.keys(node).sort().forEach(key => {{
                const item = node[key];
                const li = document.createElement('li');
                const hasChildren = Object.keys(item.children).length > 0;

                if (hasChildren) {{
                    const span = document.createElement('span');
                    span.className = 'caret';
                    span.onclick = function() {{
                        this.parentElement.querySelector('.nested').classList.toggle('active');
                        this.classList.toggle('caret-down');
                    }};
                    li.appendChild(span);
                }} else {{
                     li.style.paddingLeft = "15px";
                }}

                if (item.tests.length > 0) {{
                   const test = item.tests[0];
                   const checkbox = document.createElement('input');
                   checkbox.type = 'checkbox';
                   checkbox.className = 'test-checkbox';
                   checkbox.checked = enabledTests.has(test.id);
                   checkbox.onchange = (e) => toggleTest(test.id, e.target.checked);
                   li.appendChild(checkbox);

                   const nameSpan = document.createElement('span');
                   nameSpan.innerText = key;
                   li.appendChild(nameSpan);

                   // const uLocs = uniqueCoverageLocs[test.id] || [];
                   // const uCount = uLocs.length;
                   // const uBadge = document.createElement('span');
                   // uBadge.className = 'unique-badge' + (uCount === 0 ? ' redundant' : '');

                   // uBadge.innerText = uCount;
                   // uBadge.title = "Unique lines covered by this test. Click to list.";

                   // if (uCount > 0) {{
                   //     uBadge.onclick = (e) => {{
                   //         e.stopPropagation();
                   //         const list = document.getElementById(`unique-list-${{test.id}}`);
                   //         if (list) list.classList.toggle('visible');
                   //     }};
                   // }}
                   // li.appendChild(uBadge);

                   // if (uCount > 0) {{
                   //     const uList = document.createElement('div');
                   //     uList.className = 'unique-list';
                   //     uList.id = `unique-list-${{test.id}}`;
                   //     uLocs.forEach(loc => {{
                   //         const item = document.createElement('div');
                   //         item.className = 'unique-item';
                   //         item.innerText = `${{loc.file}}:${{loc.line}}`;
                   //         item.title = `Jump to ${{loc.file}}:${{loc.line}}`;
                   //         item.onclick = (e) => {{
                   //             e.stopPropagation();
                   //             jumpToLine(loc.file, loc.line);
                   //         }};
                   //         uList.appendChild(item);
                   //     }});
                   //     li.appendChild(uList);
                   // }}
                }} else {{
                    li.appendChild(document.createTextNode(key));
                    if (hasChildren) li.appendChild(createTreeInfo(item.children, key));
                }}

                ul.appendChild(li);
            }});
            return ul;
        }}

        function toggleTest(id, matches) {{
            if (matches) enabledTests.add(id);
            else enabledTests.delete(id);
            updateFullMask();
        }}

        function toggleAll(enable) {{
            if (enable) data.tests.forEach(t => enabledTests.add(t.id));
            else enabledTests.clear();
            document.querySelectorAll('.test-checkbox').forEach(cb => cb.checked = enable);
            updateFullMask();
        }}

        function jumpToLine(filename, lineNum) {{
            if (activeFile !== filename) {{
                activeFile = filename;
                renderFileList();
                renderSource(filename);
            }}
            const el = document.getElementById(`line-${{lineNum}}`);
            if (el) {{
                el.scrollIntoView({{block: 'center', behavior: 'smooth'}});
                el.classList.add('highlight-flash');
                setTimeout(() => el.classList.remove('highlight-flash'), 2000);
            }}
        }}

        function renderSource(filename) {{
            const container = document.getElementById('source-content');
            container.innerHTML = "";
            const file = data.files[filename];

            file.source.forEach((lineBytes, index) => {{
                const lineNum = index + 1;
                const lineData = file.lines[String(lineNum)];
                let className = 'code-line';
                let messages = [];
                let symbol = '&nbsp;';
                let testCount = 0;

                if (lineData) {{
                    const result = getLineStatus(lineData);
                    if (result.status === 'full') {{ className += ' c-full'; symbol = '+'; }}
                    else if (result.status === 'partial') {{ className += ' c-partial'; messages = result.messages.map(mid => data.msg_pool[mid]); symbol = '!'; }}
                    else if (result.status === 'exempt') {{ className += ' c-exempt'; symbol = '*'; }}
                    else if (result.status === 'unknown') {{ className += ' c-unknown'; symbol = '?'; }}
                    else {{ className += ' c-none'; symbol = '-'; }}

                    const mask = BigInt("0x" + lineData.mask_full) | BigInt("0x" + lineData.mask_partial);
                    let temp = mask;
                    while (temp > 0n) {{ if ((temp & 1n) === 1n) testCount++; temp >>= 1n; }}
                }} else {{ symbol = ''; }}

                if (messages.length > 0) className += ' has-messages';

                const div = document.createElement('div');
                div.className = className;
                div.id = `line-${{lineNum}}`;

                let actions = '';
                if (testCount > 0) {{
                    actions = `<div class="line-count" onclick="toggleDetails(${{lineNum}}, '${{filename}}', 'tests', event)">${{testCount}}</div>`;
                }} else {{
                    actions = `<div class="line-count" style="cursor:default; color:#ccc;">-</div>`;
                }}

                div.innerHTML = `
                    <div class="line-num">${{lineNum}}</div>
                    <div class="line-status">${{symbol}}</div>
                    ${{actions}}
                    <div class="line-content">${{escapeHtml(lineBytes)}}</div>
                `;

                // Clicking message code lines toggles messages
                if (messages.length > 0) {{
                    const contentDiv = div.querySelector('.line-content');
                    contentDiv.onclick = (e) => toggleDetails(lineNum, filename, 'msgs', e);
                }}

                container.appendChild(div);

                // Inline container for both messages and tests
                const detailDiv = document.createElement('div');
                detailDiv.className = 'inline-details';
                detailDiv.id = `details-${{lineNum}}`;
                // Populate messages if any
                if (messages.length > 0) {{
                     let html = '<div style="font-weight:bold; margin-bottom:5px;">Messages:</div>';
                     html += messages.map(m => `<div class="msg-box">${{escapeHtml(m)}}</div>`).join('');
                     const msgContainer = document.createElement('div');
                     msgContainer.id = `msgs-${{lineNum}}`;
                     msgContainer.style.display = 'none';
                     msgContainer.innerHTML = html;
                     detailDiv.appendChild(msgContainer);
                }}
                // Placeholder for tests
                const testContainer = document.createElement('div');
                testContainer.id = `tests-${{lineNum}}`;
                testContainer.style.display = 'none'; // Hidden by default
                detailDiv.appendChild(testContainer);

                container.appendChild(detailDiv);
            }});
        }}

        function toggleDetails(lineNum, filename, type, event) {{
            // Prevent event bubbling if needed
            if(event) event.stopPropagation();

            const detailDiv = document.getElementById(`details-${{lineNum}}`);
            const targetContainer = document.getElementById(`${{type}}-${{lineNum}}`);

            if (!detailDiv || !targetContainer) return;

            // Logic:
            // If opening 'tests', and 'msgs' is open, keep detailDiv open.
            // If toggling 'tests', flip its display.

            // 1. Populate data if needed (for tests)
            if (type === 'tests' && targetContainer.innerHTML === '') {{
                // Find tests
               const file = data.files[filename];
               const lineData = file.lines[String(lineNum)];
               const mask = BigInt("0x" + lineData.mask_full) | BigInt("0x" + lineData.mask_partial);
               const coveringTests = [];
               data.tests.forEach(test => {{
                   if ((mask & (1n << BigInt(test.id))) !== 0n) coveringTests.push(test.name);
               }});

               let html = '<div style="font-weight:bold; margin-bottom:5px;">Covering Tests:</div>';
               html += coveringTests.length > 0 ?
                   coveringTests.map(t => `<div class="test-box">${{t}}</div>`).join('') : '<div class="test-box">None</div>';
               targetContainer.innerHTML = html;
            }}

            // 2. Toggle display
            const currentlyVisible = (targetContainer.style.display === 'block');
            targetContainer.style.display = currentlyVisible ? 'none' : 'block';

            // 3. Show/Hide main container
            // If ANY child is visible, show container. Else hide.
            let anyVisible = false;
            for (let i=0; i < detailDiv.children.length; i++) {{
                if (detailDiv.children[i].style.display === 'block') anyVisible = true;
            }}

            if (anyVisible) detailDiv.classList.add('visible');
            else detailDiv.classList.remove('visible');
        }}

        function escapeHtml(text) {{
            if (!text) return "";
            if (typeof text !== 'string') text = String(text);
            return text.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#039;");
        }}

        init();
    </script>
</body>
</html>
    """

    with open(os.path.join(REPORT_DIR, "index.html"), 'w') as f:
        f.write(html_content)

if __name__ == "__main__":
    main()
