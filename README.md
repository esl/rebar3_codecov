rebar3_codecov
=====

Parse .coverdata files to json

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_codecov, ".*", {git, "git@host:user/rebar3_codecov.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_codecov
    ===> Fetching rebar3_codecov
    ===> Compiling rebar3_codecov
    <Plugin Output>
