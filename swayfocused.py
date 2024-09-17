#!/usr/bin/env python

import asyncio
from i3ipc.aio import Connection
import re

async def main():
    i3 = await Connection().connect()
    outputs = await i3.get_outputs()
    for idx, output in enumerate(outputs, start=1):
        if output.focused:
            print(idx)
            return

focus = asyncio.run(main())
