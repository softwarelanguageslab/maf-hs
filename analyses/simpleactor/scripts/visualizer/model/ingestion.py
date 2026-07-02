"""Threaded standard-input reader that streams raw lines to a consumer."""

from __future__ import annotations

import threading
from typing import Callable, TextIO

LineHandler = Callable[[str], None]


class StdinReader:
    """Read a text stream line-by-line on a background daemon thread.

    Each line is forwarded to ``on_line`` from the reader thread; an optional
    ``on_eof`` callback fires once the stream is exhausted. The handler is
    responsible for any thread-safe handoff to a GUI thread.
    """

    def __init__(
        self,
        on_line: LineHandler,
        on_eof: Callable[[], None] | None = None,
        stream: TextIO | None = None,
    ) -> None:
        self._on_line = on_line
        self._on_eof = on_eof
        self._stream = stream
        self._thread: threading.Thread | None = None
        self._stop = threading.Event()

    def start(self) -> None:
        """Begin reading on a daemon thread."""
        if self._thread is not None:
            return
        self._thread = threading.Thread(target=self._run, daemon=True)
        self._thread.start()

    def stop(self) -> None:
        """Request the reader loop to terminate."""
        self._stop.set()

    def _resolve_stream(self) -> TextIO:
        """Return the configured stream or the process standard input."""
        if self._stream is not None:
            return self._stream
        import sys

        return sys.stdin

    def _run(self) -> None:
        """Consume the stream until EOF, a stop request, or a read error."""
        stream = self._resolve_stream()
        try:
            for line in stream:
                if self._stop.is_set():
                    break
                self._on_line(line)
        except (ValueError, OSError):
            pass
        except Exception as e:
            print(f"an error {e}")
        finally:
            if self._on_eof is not None and not self._stop.is_set():
                self._on_eof()
