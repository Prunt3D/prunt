import uPlot from "./uPlot.esm.js";

type AxisName = "X" | "Y" | "Z" | "E";
type SampleResponse = {
  axes: AxisName[];
  dt_s: number;
  first_sequence: number;
  last_sequence: number;
  samples: number;
  t: number[];
  position: number[][];
  velocity: number[][];
  acceleration: number[][];
  jerk: number[][];
  snap: number[][];
  crackle: number[][];
};

const derivativeNames = ["position", "velocity", "acceleration", "jerk", "snap", "crackle"] as const;
type DerivativeName = (typeof derivativeNames)[number];

const derivativeTitles: Record<DerivativeName, string> = {
  position: "Position",
  velocity: "Velocity",
  acceleration: "Acceleration",
  jerk: "Jerk",
  snap: "Snap",
  crackle: "Crackle",
};
const derivativeUnits: Record<DerivativeName, string> = {
  position: "mm",
  velocity: "mm/s",
  acceleration: "mm/s^2",
  jerk: "mm/s^3",
  snap: "mm/s^4",
  crackle: "mm/s^5",
};
const axisColors = ["#116b5f", "#7b4ca0", "#c2701e", "#3869b1"];
const commandForm = document.querySelector<HTMLFormElement>("#command-form")!;
const commandInput = document.querySelector<HTMLInputElement>("#command-input")!;
const sampleSummary = document.querySelector<HTMLElement>("#sample-summary")!;
const xySummary = document.querySelector<HTMLElement>("#xy-summary")!;
const xyPlotElement = document.querySelector<HTMLElement>("#xy-plot")!;
const derivativePlotsElement = document.querySelector<HTMLElement>("#derivative-plots")!;

let xyPlot: any | undefined;
let derivativePlots: any[] = [];
let lastSnapshotKey = "";

function paddedRange(_plot: any, dataMin: number | null, dataMax: number | null): [number, number] {
  if (dataMin === null || dataMax === null || !Number.isFinite(dataMin) || !Number.isFinite(dataMax)) {
    return [0, 1];
  }

  if (dataMin === dataMax) {
    const pad = Math.max(Math.abs(dataMin) * 0.05, 1);
    return [dataMin - pad, dataMax + pad];
  }

  const pad = (dataMax - dataMin) * 0.06;
  return [dataMin - pad, dataMax + pad];
}

function dataExtent(values: number[] | undefined): [number, number] | undefined {
  if (!values || values.length === 0) {
    return undefined;
  }

  let min = Number.POSITIVE_INFINITY;
  let max = Number.NEGATIVE_INFINITY;
  for (const value of values) {
    if (Number.isFinite(value)) {
      min = Math.min(min, value);
      max = Math.max(max, value);
    }
  }

  return min === Number.POSITIVE_INFINITY ? undefined : [min, max];
}

function closeEnough(left: number, right: number) {
  return Math.abs(left - right) <= Math.max(1e-9, Math.abs(right) * 1e-9);
}

function scaleMatchesRange(plot: any, scale: "x" | "y", range: [number, number] | undefined) {
  if (!range) {
    return true;
  }

  const current = plot.scales[scale];
  return closeEnough(current.min, range[0]) && closeEnough(current.max, range[1]);
}

function paddedExtentRange(plot: any, values: number[] | undefined) {
  const extent = dataExtent(values);
  return extent ? paddedRange(plot, extent[0], extent[1]) : paddedRange(plot, null, null);
}

function formatDt(dt: number) {
  if (dt < 0.001) {
    return `${(dt * 1_000_000).toPrecision(4)} us`;
  }

  return `${dt.toPrecision(4)} s`;
}

function sizeOf(element: HTMLElement, fallbackHeight: number) {
  return {
    width: Math.max(320, element.clientWidth),
    height: Math.max(fallbackHeight, element.clientHeight || fallbackHeight),
  };
}

function xyData(data: SampleResponse): any[] {
  return [null, [data.position[0] ?? [], data.position[1] ?? []]];
}

function updateXYPlot(data: SampleResponse) {
  if (!xyPlot) {
    xyPlot = makeXYPlot(data);
    return;
  }

  const oldPath = xyPlot.data[1] as [number[], number[]];
  const oldXRange = paddedExtentRange(xyPlot, oldPath[0]);
  const oldYRange = paddedExtentRange(xyPlot, oldPath[1]);
  const wasZoomed =
    !scaleMatchesRange(xyPlot, "x", oldXRange) || !scaleMatchesRange(xyPlot, "y", oldYRange);
  const xScale = { min: xyPlot.scales.x.min, max: xyPlot.scales.x.max };
  const yScale = { min: xyPlot.scales.y.min, max: xyPlot.scales.y.max };

  xyPlot.setData(xyData(data), !wasZoomed);
  if (wasZoomed) {
    xyPlot.batch((plot: any) => {
      plot.setScale("x", xScale);
      plot.setScale("y", yScale);
    });
  }
}

function updateTimeSeriesPlot(plot: any, data: number[][]) {
  const oldTime = plot.data[0] as number[] | undefined;
  const oldRange = dataExtent(oldTime);
  const wasZoomed = !scaleMatchesRange(plot, "x", oldRange);
  const xScale = { min: plot.scales.x.min, max: plot.scales.x.max };

  plot.setData(data, !wasZoomed);
  if (wasZoomed) {
    plot.setScale("x", xScale);
  }
}

function makeXYPlot(data: SampleResponse) {
  const size = sizeOf(xyPlotElement, 430);
  return new uPlot(
    {
      ...size,
      mode: 2,
      cursor: { drag: { x: true, y: true } },
      legend: { show: false },
      scales: {
        x: { time: false, range: paddedRange },
        y: { range: paddedRange },
      },
      axes: [
        { label: "X mm" },
        { label: "Y mm" },
      ],
      series: [
        {},
        {
          label: "Y",
          stroke: axisColors[1],
          width: 2,
          points: { show: false },
        },
      ],
    },
    xyData(data),
    xyPlotElement,
  );
}

function makeDerivativePlot(data: SampleResponse, derivative: DerivativeName, target: HTMLElement) {
  const size = sizeOf(target, 260);
  const series = data.axes.map((axis, index) => ({
    label: axis,
    stroke: axisColors[index],
    width: 1.5,
    points: { show: false },
  }));

  return new uPlot(
    {
      ...size,
      cursor: { drag: { x: true, y: false } },
      scales: {
        x: { time: false },
        y: { auto: true, range: paddedRange },
      },
      axes: [
        { label: "s" },
        { label: derivativeUnits[derivative] },
      ],
      series: [{}, ...series],
    },
    derivativeData(data, derivative),
    target,
  );
}

function derivativeData(data: SampleResponse, derivative: DerivativeName): number[][] {
  return [
    data.t,
    ...data.axes.map((_axis, axisIndex) => data[derivative][axisIndex] ?? []),
  ];
}

function rebuildDerivativePlots(data: SampleResponse) {
  derivativePlots.forEach((plot) => plot.destroy());
  derivativePlots = [];
  derivativePlotsElement.replaceChildren();

  derivativeNames.forEach((derivative) => {
    const panel = document.createElement("section");
    panel.className = "derivative-panel";

    const heading = document.createElement("h3");
    heading.textContent = derivativeTitles[derivative];

    const plotElement = document.createElement("div");
    plotElement.className = "plot";

    panel.append(heading, plotElement);
    derivativePlotsElement.append(panel);
    derivativePlots.push(makeDerivativePlot(data, derivative, plotElement));
  });
}

function updatePlots(data: SampleResponse) {
  sampleSummary.textContent =
    `${data.samples} samples, sequence ${data.first_sequence} to ${data.last_sequence}, dt ${formatDt(data.dt_s)}`;
  xySummary.textContent = data.samples > 0 ? `${data.axes[0]}/${data.axes[1]} path` : "No motion yet";

  if (!xyPlot) {
    updateXYPlot(data);
    rebuildDerivativePlots(data);
    return;
  }

  updateXYPlot(data);
  if (derivativePlots.length !== derivativeNames.length) {
    rebuildDerivativePlots(data);
  } else {
    derivativePlots.forEach((plot, index) => {
      updateTimeSeriesPlot(plot, derivativeData(data, derivativeNames[index]));
    });
  }
}

async function pollSamples() {
  try {
    const response = await fetch("/extras/position_samples.json", { cache: "no-store" });
    const text = await response.text();
    const data = JSON.parse(text.trim()) as SampleResponse;
    const snapshotKey = `${data.first_sequence}:${data.last_sequence}:${data.samples}`;
    if (snapshotKey !== lastSnapshotKey) {
      lastSnapshotKey = snapshotKey;
      updatePlots(data);
    }
  } catch (error) {
    sampleSummary.textContent = error instanceof Error ? error.message : String(error);
  }
}

commandForm.addEventListener("submit", async (event) => {
  event.preventDefault();
  const command = commandInput.value.trim();
  if (!command) {
    return;
  }

  const submitButton = commandForm.querySelector<HTMLButtonElement>("button")!;
  submitButton.disabled = true;
  try {
    const response = await fetch("/run-command", { method: "POST", body: command });
    if (!response.ok) {
      throw new Error(await response.text());
    }
  } catch (error) {
    sampleSummary.textContent = error instanceof Error ? error.message : String(error);
  } finally {
    submitButton.disabled = false;
  }
});

window.addEventListener("resize", () => {
  if (xyPlot) {
    const size = sizeOf(xyPlotElement, 430);
    xyPlot.setSize(size);
  }

  derivativePlots.forEach((plot) => {
    const target = plot.root.parentElement as HTMLElement;
    plot.setSize(sizeOf(target, 260));
  });
});

void pollSamples();
window.setInterval(() => void pollSamples(), 750);
