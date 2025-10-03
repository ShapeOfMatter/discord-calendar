## Expose Discord scheduled events as iCal or json

This server will read all the "events" in a Discord server and expose them to the public.
Actually rendering them as calendar events can be done by basically any calendar service, app, or library.

Intended behavior:

- [x] read bot token from config file
- [x] bad paths give 404
- [x] requests for indadmissible guilds give 403
- [x] get guild from path
- [x] forward discord errors
- [x] handle both kinds of event ids
- [x] serve json
