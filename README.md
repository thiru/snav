# snav

**snav** stands for screen navigator. It is a window, workspace and monitor navigator for X.

The motivation for writing this app is so I could navigate windows, workspaces and monitors as easily as with tiling window managers like [i3](https://github.com/i3/i3), [bspwm](https://github.com/baskerville/bspwm), etc. but without having to use one.

There's a lot to like about tiling window managers but I find there are also many drawbacks. E.g. there's a lot of extra things you need to setup manually that most desktop environments provide out-of-the-box (working with multiple monitors, a good panel, desktop themes, etc.). It's also generally difficult to work with and nagivate between floating windows. E.g. bspwm requires you to create explicit rules so that some windows behave as you'd expect (e.g. Team Viewer). And this process is tedious enough that I often just don't get around to it.

Perhaps my favourite thing about tiling window managers is how easy it is to traverse workspaces and windows. Windows can be traversed according to how they are visually laid out on the screen. I find this much easier than working with ALT-TAB (beyond just switching to the last window). This is the primary goal of this app.

## Features

* Navigate to a specific workspace (by number)
* Navigate to a workspace by relative position (left, right)
* Navigate to the last viewed workspace
* Focus a window by relative position (up, down, left, right)
* Focus a specific monitor (by number) *NOT YET IMPLEMENTED*
* Focus a monitor by relative position (up, down, left, right) *NOT YET IMPLEMENTED*

## Requirements

* wmctrl
* [xdotool](https://github.com/jordansissel/xdotool)
* Common Lisp

## Running

Just grab the snav executable from releases. See the *Config* section for command examples. You can also just run *snav help*.

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

## Building

I prefer to use [roswell](https://github.com/roswell/roswell) to easily build an executable from source. The [build script](./build.sh) leverages this.
