# This script monitors the output of the SimpleActor analysis,
# and looks for components that have a high self-time. It 
# exits with a zero exit code if a component with a self-time of > 20ms has
# been found, and quits with a nonzero exit code otherwise.
#
# The scripts/visualizer/ utility is used for its event processing functionality, and is assumed to be installed on the Python path (see properties/performance_dd.nu)

import sys

from visualizer.model.events import parse_event, IntraEnded
from visualizer.model.ingestion import StdinReader

outlier_self_time_found = False

def consume_line(line):
    global outlier_self_time_found
    event = parse_event(line)
    match event:
        case IntraEnded():
            duration_ms = event.duration * 10**3
            if duration_ms > 40:
                outlier_self_time_found = True
        case _:
            pass

def exit_program(*args, **kwargs):
    print("Exiting program...")
    sys.exit(0 if outlier_self_time_found else 1)


reader  = StdinReader(on_line=consume_line,
                      on_eof=exit_program)
reader._run()
