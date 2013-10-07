rebar-deps
==

rebar-ver is a tool which aids to help retrive information about deps from
rebar.config.

Example usage
==

Consider we have rebar.config:
```erlang
{erl_opts, [
    {parse_transform, lager_transform}
]}.

{deps, [
    {lager, "2.0.0",
     {git, "https://github.com/EchoTeam/lager", "12433c0deca18a7475215b01f226a213cbdc2b09"}},
    {harlson_proto, ".*",
     {git, "git@github.com:EchoTeam/harlson-proto.git", "master"}}
]}.
```

We can get summary information about deps:
```
$ ./rebar-deps rebar.config
lager|12433c0deca18a7475215b01f226a213cbdc2b09|https://github.com/EchoTeam/lager
harlson_proto|master|git@github.com:EchoTeam/harlson-proto.git 
```

Now, using piping, grep and cut we can get any field.

Typical usecase is to get a revision of application
```
$ ./rebar-deps rebar.config | grep "^lager|" | cut -d'|' -f2
12433c0deca18a7475215b01f226a213cbdc2b09
```

Also we can get version more easy:
```
$ ./rebar-deps rebar.config lager
12433c0deca18a7475215b01f226a213cbdc2b09
```

We can also set a new version for application:
```
$ ./rebar-deps rebar.config lager 777
$ ./rebar-deps rebar.config lager
777
$ cat rebar.config
{erl_opts, [
    {parse_transform, lager_transform}
]}.

{deps, [
    {lager, "2.0.0",
     {git, "https://github.com/EchoTeam/lager", "777"}},
    {harlson_proto, ".*",
     {git, "git@github.com:EchoTeam/harlson-proto.git", "master"}}
]}.
```


Limitations
==

* Working with tags (i.e. {git, "https://repo/name", {tag, "thetag"}}) is
  dangerous.

<!--
    vim: ts=4 sw=4 et ft=Markdown tw=80
-->
