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
    { rebar3_codecov, {git,
                          "https://github.com/zofpolkowska/rebar3_codecov.git", {branch, "master"}}}
]}.

{provider_hooks,
 [
  {post, [{cover, {codecov, analyze}}]}
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

Go to https://codecov.io and add your repository to see the results of code coverage
