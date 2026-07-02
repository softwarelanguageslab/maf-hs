"""Typed representations of analysis logging events and their JSON parser."""

from __future__ import annotations

import json
from dataclasses import dataclass


@dataclass(frozen=True)
class Position:
    """A single line/column location inside a source file."""

    line: int
    column: int

    @classmethod
    def from_json(cls, data: dict) -> "Position":
        """Build a :class:`Position` from its serialized object."""
        return cls(line=int(data["line"]), column=int(data["column"]))


@dataclass(frozen=True)
class Span:
    """Source-code region that uniquely identifies an analysis component."""

    filename: str
    start: Position
    end: Position

    @classmethod
    def from_json(cls, data: dict) -> "Span":
        """Build a :class:`Span` from its serialized ``contents`` object."""
        return cls(
            filename=str(data["filename"]),
            start=Position.from_json(data["startPosition"]),
            end=Position.from_json(data["endPosition"]),
        )

    @property
    def display_name(self) -> str:
        """Human-readable identifier ``filename:startLine:startCol-endLine:endCol``."""
        return (
            f"{self.filename}:"
            f"{self.start.line}:{self.start.column}-"
            f"{self.end.line}:{self.end.column}"
        )


@dataclass(frozen=True)
class IntraStarted:
    """Start of an intra-analysis for the given component span."""

    span: Span


@dataclass(frozen=True)
class IntraEnded:
    """End of an intra-analysis for the given component span."""

    span: Span
    # Duration in seconds
    duration: int


@dataclass(frozen=True)
class PreBranch:
    """Marker emitted before a branch is evaluated."""


@dataclass(frozen=True)
class PostBranch:
    """Marker emitted after a branch has been evaluated."""


Event = IntraStarted | IntraEnded | PreBranch | PostBranch


def parse_event(line: str):
    """Parse one newline-delimited JSON record into an :data:`Event`.

    Returns ``None`` for blank lines, malformed JSON or unrecognized records so
    that a streaming consumer can skip bad input without crashing.
    """
    text = line.strip()
    if not text:
        return None
    try:
        record = json.loads(text)
    except (json.JSONDecodeError, ValueError):
        return None
    if not isinstance(record, dict):
        return None

    tag = record.get("tag")
    try:
        if tag == "IntraStarted":
            return IntraStarted(Span.from_json(record["contents"]))
        if tag == "IntraEnded":
            span = Span.from_json(record["contents"][0])
            duration = record["contents"][1] / 10**9
            return IntraEnded(span, duration)
        if tag == "PreBranch":
            return PreBranch()
        if tag == "PostBranch":
            return PostBranch()
    except (KeyError, TypeError, ValueError):
        return None
    return None
