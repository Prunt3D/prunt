#!/usr/bin/env python3
import argparse
import json
from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec


EVENT_LIST_ROWS = 7

DERIVATIVES = [
    ("position", "Position", "mm"),
    ("velocity", "Velocity", "mm/s"),
    ("acceleration", "Acceleration", "mm/s^2"),
    ("jerk", "Jerk", "mm/s^3"),
    ("snap", "Snap", "mm/s^4"),
    ("crackle", "Crackle", "mm/s^5"),
]

EVENT_COLORS = {
    "gcode": "#1f77b4",
    "machine": "#2ca02c",
    "motor": "#9467bd",
    "fan": "#17becf",
    "heater": "#d62728",
    "thermistor": "#ff7f0e",
    "input_switch": "#8c564b",
    "tachometer": "#e377c2",
    "board_temperature": "#7f7f7f",
    "cancel": "#111111",
}


def event_time(event):
    t = event.get("t_s")
    if t is None:
        return None
    return float(t)


def short_text(value, limit):
    text = str(value)
    if len(text) <= limit:
        return text
    return text[: max(0, limit - 3)] + "..."


def format_value(value):
    text = str(value)
    try:
        number = float(text)
    except ValueError:
        return text
    return f"{number:.6g}"


def event_label(event):
    parts = [event.get("kind", ""), event.get("label", "")]
    target = event.get("target", "")
    value = event.get("value", "")
    if target:
        parts.append(target)
    if value:
        parts.append(format_value(value))
    return " ".join(part for part in parts if part)


def format_event_row(event):
    t = event_time(event)
    time_text = "------" if t is None else f"{t:8.5f}"
    return short_text(f"{time_text}s  {event_label(event)}", 72)


def format_event_detail(event):
    lines = []
    t = event_time(event)
    if t is not None:
        lines.append(f"time: {t:.6f} s")
    for key in ("kind", "label", "target", "value", "command_index"):
        value = event.get(key)
        if value not in (None, ""):
            lines.append(f"{key}: {short_text(format_value(value), 92)}")
    return "\n".join(lines) if lines else "(empty event)"


def add_event_markers(ax, events, artists):
    ymin, ymax = ax.get_ylim()
    for idx, event in enumerate(events):
        t = event_time(event)
        if t is None:
            continue
        line = ax.axvline(t, color="#7a3b2e", alpha=0.25, linewidth=1, picker=5)
        artists[line] = idx
    ax.set_ylim(ymin, ymax)


def sample_count(trace):
    if isinstance(trace.get("samples"), int):
        return trace["samples"]
    return len(trace.get("t", []))


def event_kinds(events):
    kinds = []
    for event in events:
        kind = event.get("kind", "event")
        if kind not in kinds:
            kinds.append(kind)
    return kinds or ["event"]


def add_event_timeline(ax, events, artists):
    kinds = event_kinds(events)
    kind_indices = {kind: idx for idx, kind in enumerate(kinds)}
    overlap_counts = {}

    for idx, event in enumerate(events):
        t = event_time(event)
        if t is None:
            continue

        kind = event.get("kind", "event")
        key = (kind, round(t, 6))
        overlap_index = overlap_counts.get(key, 0)
        overlap_counts[key] = overlap_index + 1
        y = kind_indices[kind] + ((overlap_index % 7) - 3) * 0.06

        color = EVENT_COLORS.get(kind, "#555555")
        (point,) = ax.plot(
            [t],
            [y],
            marker="o",
            markersize=6,
            linestyle="",
            color=color,
            alpha=0.9,
            picker=7,
        )
        artists[point] = idx

    ax.set_title("Event Timeline")
    ax.set_xlabel("s")
    ax.set_yticks(range(len(kinds)), kinds)
    ax.set_ylim(-0.55, len(kinds) - 0.45)
    ax.grid(True, axis="x", alpha=0.25)
    ax.grid(True, axis="y", alpha=0.12)


def set_time_limits(ax, t, events):
    max_t = 0.0
    if t:
        max_t = max(max_t, max(float(value) for value in t))
    for event in events:
        event_t = event_time(event)
        if event_t is not None:
            max_t = max(max_t, event_t)
    pad = max(max_t * 0.03, 0.01)
    ax.set_xlim(-pad, max_t + pad)


def plot_trace(path):
    trace = json.loads(Path(path).read_text())
    axes = trace["axes"]
    events = trace.get("events", [])
    t = trace.get("t", [])
    artist_events = {}

    fig = plt.figure(figsize=(16, 11), constrained_layout=True)
    fig.canvas.manager.set_window_title(trace.get("name", Path(path).name))

    grid = GridSpec(
        5,
        2,
        figure=fig,
        height_ratios=[1.25, 0.8, 1.0, 1.0, 1.0],
        width_ratios=[1.0, 1.45],
    )
    xy_ax = fig.add_subplot(grid[0, 0])
    event_ax = fig.add_subplot(grid[0, 1])
    timeline_ax = fig.add_subplot(grid[1, :])
    derivative_axes = [fig.add_subplot(grid[2 + i // 2, i % 2]) for i in range(len(DERIVATIVES))]

    pos = trace.get("position", [])
    if len(pos) >= 2:
        xy_ax.plot(pos[0], pos[1], linewidth=1.5)
    xy_ax.set_title("XY Toolpath")
    xy_ax.set_xlabel("X mm")
    xy_ax.set_ylabel("Y mm")
    xy_ax.axis("equal")
    xy_ax.grid(True, alpha=0.25)

    event_ax.set_title("Event Inspector")
    event_ax.axis("off")
    detail_text = event_ax.text(
        0.0,
        1.0,
        f"{sample_count(trace)} samples, {len(events)} events",
        transform=event_ax.transAxes,
        va="top",
        fontsize=9,
        family="monospace",
        bbox={"boxstyle": "round,pad=0.35", "facecolor": "#f7f7f7", "edgecolor": "#aaaaaa"},
    )
    list_artists = []
    state = {"selected": None, "offset": 0}

    add_event_timeline(timeline_ax, events, artist_events)
    set_time_limits(timeline_ax, t, events)

    time_axes = [timeline_ax]
    for ax, (key, title, unit) in zip(derivative_axes, DERIVATIVES):
        values = trace.get(key, [])
        for axis_name, series in zip(axes, values):
            ax.plot(t, series, label=axis_name, linewidth=1)
        ax.set_title(title)
        ax.set_xlabel("s")
        ax.set_ylabel(unit)
        ax.grid(True, alpha=0.25)
        ax.legend(loc="upper right", fontsize=8, ncols=2)
        add_event_markers(ax, events, artist_events)
        set_time_limits(ax, t, events)
        time_axes.append(ax)

    selection_lines = []
    for ax in time_axes:
        line = ax.axvline(0.0, color="#111111", alpha=0.0, linewidth=1.6)
        selection_lines.append(line)

    def render_event_list():
        for artist in list_artists:
            artist_events.pop(artist, None)
            artist.remove()
        list_artists.clear()

        if not events:
            return

        max_offset = max(0, len(events) - EVENT_LIST_ROWS)
        state["offset"] = max(0, min(state["offset"], max_offset))
        start = state["offset"]
        end = min(len(events), start + EVENT_LIST_ROWS)

        header = event_ax.text(
            0.0,
            0.52,
            f"events {start + 1}-{end} of {len(events)}",
            transform=event_ax.transAxes,
            va="top",
            fontsize=8,
            color="#555555",
        )
        list_artists.append(header)

        for row_index, event_index in enumerate(range(start, end)):
            event = events[event_index]
            is_selected = event_index == state["selected"]
            row = event_ax.text(
                0.0,
                0.44 - row_index * 0.065,
                format_event_row(event),
                transform=event_ax.transAxes,
                va="top",
                fontsize=8,
                family="monospace",
                picker=True,
                clip_on=True,
                bbox=(
                    {"boxstyle": "square,pad=0.18", "facecolor": "#e9eef7", "edgecolor": "#c5d1e8"}
                    if is_selected
                    else None
                ),
            )
            artist_events[row] = event_index
            list_artists.append(row)

    def show_event(event_index):
        state["selected"] = event_index
        event = events[event_index]
        detail_text.set_text(format_event_detail(event))
        event_t = event_time(event)
        for line in selection_lines:
            if event_t is None:
                line.set_alpha(0.0)
            else:
                line.set_xdata([event_t, event_t])
                line.set_alpha(0.8)
        render_event_list()
        fig.canvas.draw_idle()

    def on_pick(event):
        event_index = artist_events.get(event.artist)
        if event_index is not None:
            show_event(event_index)

    def on_scroll(event):
        if event.inaxes != event_ax or len(events) <= EVENT_LIST_ROWS:
            return
        direction = -1 if event.step > 0 else 1
        state["offset"] += direction * EVENT_LIST_ROWS
        render_event_list()
        fig.canvas.draw_idle()

    if events:
        show_event(0)
    else:
        render_event_list()
    fig.canvas.mpl_connect("pick_event", on_pick)
    fig.canvas.mpl_connect("scroll_event", on_scroll)

    plt.show()


def main():
    parser = argparse.ArgumentParser(description="View a Prunt integration trace.")
    parser.add_argument("trace", help="Path to an integration trace JSON file.")
    args = parser.parse_args()
    plot_trace(args.trace)


if __name__ == "__main__":
    main()
