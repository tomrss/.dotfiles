#!/usr/bin/env python3

# Replicate the behaviour of Alt+Tab within a workspace
# Source: https://gist.githubusercontent.com/SidharthArya/f4d80c246793eb61be0ae928c9184406/raw/d6cddb949e0508a816969bac305d2d7eac117372/alttab

import sys
import json
import subprocess

direction=bool(sys.argv[1] == 't' or sys.argv[1] == 'T')
swaymsg = subprocess.run(['swaymsg', '-t', 'get_tree'], stdout=subprocess.PIPE)
data = json.loads(swaymsg.stdout)
current = data["nodes"][1]["current_workspace"]
workspace = int(data["nodes"][1]["current_workspace"])-1
roi = data["nodes"][1]["nodes"][workspace]
temp = roi
windows = list()

def getNextWindow():
    if focus < len(windows) - 1:
        return focus+1
    else:
        return 0

def getPrevWindow():
    if focus > 0:
        return focus-1
    else:
        return len(windows)-1



def makelist(temp):
    for nodes in "floating_nodes", "nodes":
        for i in range(len(temp[nodes])):
            if temp[nodes][i]["name"] is None:
               makelist(temp[nodes][i])
            else:
               windows.append(temp[nodes][i])

def focused(temp_win):
    for i in range(len(temp_win)):
        if temp_win[i]["focused"] == True:
           return i

makelist(temp)
# print(len(windows))
focus = focused(windows)
if str(sys.argv[1]) == 't' or str(sys.argv[1]) == 'T':
    print(windows[getNextWindow()]["id"])
else:
    print(windows[getPrevWindow()]["id"])
