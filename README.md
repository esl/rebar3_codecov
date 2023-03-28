rebar3_codecov
=====

A rebar3 plugin which converts .coverdata files to JSON format, compatible
with https://codecov.io.

Use
---

Add the plugin to your rebar config, register it as a
post-hook and enable code coverage.

```
{plugins, [
    {rebar3_codecov, "0.4.0"}
]}.

{provider_hooks,
 [
  %% Use ct, eunit, or both if you test your project with both frameworks.
  {post, [{ct, {codecov, analyze}}]},
  {post, [{eunit, {codecov, analyze}}]}
 ]}.

{cover_enabled, true}.
```

Configure Travis according to https://docs.travis-ci.com/user/getting-started/

The minimum .travis.yml should look more or less like the following:

```
language: erlang

otp_release:
    - 20.3

script:
    rebar3 ct

install:
    travis_retry pip install --user codecov

after_success:
    codecov
```

Alternatively, you can call the plugin directly.

```
rebar3 codecov analyze path="_build/test/cover"
```

By default the plugin will look for the *.coverdata files in
_build/test/cover directory. If you want to specify a different location,
add to your rebar.config the list of directories

```
{codecov_opts,
 [
  {path, ["path/to/*.coverdata/files"]}
 ]}.
```

Go to https://codecov.io and add your repository to see the results of code coverage
