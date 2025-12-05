#!/usr/bin/env python3
"""CGI entry point for Sanskrit Heritage Platform.

This script is invoked by GoldenDict or other applications to process Sanskrit text.
It replaces the PowerShell script arg.ps1 with a Python-based CGI handler.

Usage:
    python main.py [word]

Where word is a Sanskrit word in Devanagari script.
"""

import sys
import os
import urllib.parse

# import cgi

from heritage.modules.interface import process_request


def parse_commandline_args():
    """Parse command line arguments (mimicking arg.ps1 behavior)."""
    env = dict(os.environ)

    # Handle command-line argument (word to analyze)
    if len(sys.argv) > 1:
        word = sys.argv[1]
        env["text"] = urllib.parse.quote(word)

    return env


def main():
    """Main entry point."""
    # Get environment (either from command line or CGI)
    if len(sys.argv) > 1:
        # Command line invocation (e.g., from GoldenDict)
        env = parse_commandline_args()
    else:
        # CGI invocation
        env = dict(os.environ)

        # Parse form data if present
        # if os.environ.get('REQUEST_METHOD') == 'POST':
        # form_data = cgi.FieldStorage()
        # for key in form_data:
        # env[key] = form_data[key].value

    # Process the request
    try:
        output = process_request(env)
        # Try to ensure UTF-8 is used for stdout. When stdout is redirected
        # on Windows it may use a locale (e.g., 'cp1252') that cannot encode
        # Devanagari characters. Prefer reconfiguring stdout to UTF-8; if
        # that's not possible, write bytes directly to the underlying buffer.
        try:
            try:
                # Python 3.7+: reconfigure if available
                sys.stdout.reconfigure(encoding="utf-8")
                sys.stdout.write(output)
            except Exception:
                raise
                # Fallback: write bytes to buffer
                sys.stdout.buffer.write(output.encode("utf-8"))
        except (UnicodeEncodeError, OSError):
            # As a last resort write bytes
            raise
            sys.stdout.buffer.write(output.encode("utf-8"))
    except Exception as e:
        # Emergency error page
        raise
        err_html = (
            "Content-Type: text/html; charset=UTF-8\n\n"
            + f"<html><body><h1>Error</h1><p>{str(e)}</p></body></html>"
        )
        try:
            try:
                sys.stdout.reconfigure(encoding="utf-8")
                sys.stdout.write(err_html)
            except Exception:
                sys.stdout.buffer.write(err_html.encode("utf-8"))
        except Exception:
            # Nothing more we can do
            pass
        sys.exit(1)


if __name__ == "__main__":
    main()
