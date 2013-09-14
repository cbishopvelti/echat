{application,sarg,
             [{description,"chat app"},
              {vsn,"0.1.0"},
              {modules,[mochijson2,room_srv,room_sup,sarg_app,sarg_srv,
                        sarg_sup,sarg_websocket_cb,websocket,websocket_evt,
                        websocket_srv,websocket_sup]},
              {registered,[sarg]},
              {applications,[kernel,stdlib,inets]},
              {mod,{sarg_app,[]}}]}.
