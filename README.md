# musicmon

Pipeline style music tracking daemon. Uses `conduit`.

Pipeline gives flexibility to change stages. Currently the source is MPD, and the sink is an SQLite database.

## setup

Use `stack`.

```
stack build
stack exec musicmon
```

The config file is `{xdg config dir}/musicmon/musicmon.cfg`.
