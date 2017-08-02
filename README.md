# snav

**snav** stands for screen navigator. It is a window, screen and workspace navigator for X.

I wrote this app so I could navigate windows, screens and workspaces as easily as with tiling window managers like [i3](https://github.com/i3/i3), [bspwm](https://github.com/baskerville/bspwm), etc. but without having to use one. I like many of the benefits of using a full desktop environment that inherintly minimilistic tiling window managers lack.

## Features

* Navigate to a specific workspace (by number)
* Navigate to a workspace by relative position (left, right)
* Navigate to the last viewed workspace
* Focus a window by relative position (up, down, left, right)
* Focus a specific screen (by number) *NOT YET IMPLEMENTED*
* Focus a screen by relative position (up, down, left, right) *NOT YET IMPLEMENTED*

## Requirements

* wmctrl
* [xdotool](https://github.com/jordansissel/xdotool)
* Common Lisp

## Building

I prefer to use [roswell](https://github.com/roswell/roswell) to easily build an executable from source:

    ros build snav.ros

## Config

I prefer to bind the snav commands to key-bindings via [sxhkd](https://github.com/baskerville/sxhkd). Here is a sample config:

    # Go to a specific workspace
    super + {1-9,0}
      snav workspace '{1-9,10}'

    # Go to the next workspace
    super + ctrl + l
      snav workspace next

    # Go to the previous workspace
    super + ctrl + h
      snav workspace previous

    # Go to last active workspace
    super + grave
      snav workspace last

    # Focus window up
    super + k
      snav focus up

    # Focus window down
    super + j
      snav focus down

    # Focus window to the left
    super + h
      snav focus left

    # Focus window to the right
    super + l
      snav focus right
