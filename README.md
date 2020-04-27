web_proto_plugin
=====

make spec proto routing file depend on rebar3_gpb_plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {web_proto_plugin, {git, "https://github.com/ngqlaw/web_proto_plugin.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 web_proto_plugin
    ===> Fetching web_proto_plugin
    ===> Compiling web_proto_plugin
    <Plugin Output>
