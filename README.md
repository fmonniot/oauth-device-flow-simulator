# OAuth Device Flow Simulator

## Documentation

The elm documentation of this project is available online at https://francois.monniot.eu/oauth-device-flow-simulator/docs/.
The `Watch` section contains the implementation of Device Flow, and is well documented to let other people understand how the flow works.

## TODO

- [x] Bring back Main.Configuration into the watch module
    - [x] Configuration type into Watch
    - [x] Client ID, polling, Client Secret into the Watch view too
    - [x] Update/Msg should be in the Watch module as well
- [x] Add a "stop polling" checkbox to the Watch module
- [x] Consider having a "polling" checkbox instead of a "stop polling" one (inverse current behavior)
- [x] Look if we can have a Log module which scroll automatically.
    Appear we can by using `elm-lang/dom` (http://package.elm-lang.org/packages/elm-lang/dom/1.1.1/Dom-Scroll)
- [x] Configure the baseUrl in the UI
- [x] Add documentation for the whole project
    - [x] Expose documentation for all modules in `elm-package.json`
    - [x] Generate documentation
    - [x] Write missing doc ;)
    - [ ] Complete this README
