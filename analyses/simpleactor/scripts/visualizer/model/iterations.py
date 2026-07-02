"""Per-component iteration tracking built from a stream of analysis events."""

from __future__ import annotations

import time
from typing import Callable, Dict, List

from ..observable import Observable
from .events import (
    Event,
    IntraEnded,
    IntraStarted,
    PostBranch,
    PreBranch,
    Span,
)


class IterationTracker:
    """Accumulate per-component branching factors from ordered events.

    An iteration for a component starts at its ``IntraStarted`` and ends at the
    matching ``IntraEnded`` or at the next ``IntraStarted`` of any component,
    whichever comes first. The branching factor of an iteration is the number of
    ``PreBranch`` events observed while it is open. ``changed`` emits a span
    whenever its branching factors change: when an iteration starts, on each
    ``PreBranch`` that grows the open iteration, and when an iteration completes,
    so callers can render the running iteration's count live. Each completed
    iteration's wall-clock duration is measured from ``clock`` (seconds).
    """

    def __init__(self, clock: Callable[[], float] = time.monotonic) -> None:
        self.changed: Observable = Observable()
        self._clock = clock
        self._completed: Dict[Span, List[int]] = {}
        self._durations: Dict[Span, List[float]] = {}
        self._iterations: Dict[Span, Int] = {}
        self._open_span: Span | None = None
        self._open_count: int = 0

    def components(self) -> List[Span]:
        """Return the spans that have a completed or in-progress iteration."""
        spans = list(self._completed.keys())
        if self._open_span is not None and self._open_span not in self._completed:
            spans.append(self._open_span)
        return spans

    def branching_factors(self, span: Span) -> List[int]:
        """Return ``span``'s branching factors in arrival order.

        The list holds every completed iteration and, when ``span`` is the
        iteration currently open, a trailing entry with its in-progress
        ``PreBranch`` count so the running iteration can be rendered live.
        """
        factors = list(self._completed.get(span, ()))
        if span == self._open_span:
            factors.append(self._open_count)
        return factors

    def durations(self, span: Span) -> List[float]:
        """Return the wall-clock duration in seconds of ``span``'s completed iterations."""
        return list(self._durations.get(span, ()))

    def consume(self, event: Event) -> None:
        """Advance the tracker state with a single event."""
        if isinstance(event, IntraStarted):
            self._iterations[event.span] = self._iterations.get(event.span, 0) + 1
            self._open_span = event.span
            self._open_count = 0
            self._open_started = self._clock()
            self.changed.emit(event.span)
        elif isinstance(event, IntraEnded):
            if self._open_span is not None and event.span == self._open_span:
                self._close_open(event.duration)
        elif isinstance(event, PreBranch):
            if self._open_span is not None:
                self._open_count += 1
                self.changed.emit(self._open_span)
        elif isinstance(event, PostBranch):
            pass

    def _close_open(self, duration = None) -> None:
        """Finalize the currently open iteration and notify subscribers."""
        if self._open_span is None:
            return
        span = self._open_span
        self._completed.setdefault(span, []).append(self._open_count)
        if duration is not None:
            self._durations.setdefault(span, []).append(duration)
        self._open_span = None
        self._open_count = 0
        self.changed.emit(span)
