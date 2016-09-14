

# Google Cloud Messaging (GCM) Simulator #

Copyright (c) 2016 Silent Circle

__Version:__ 0.9.0

__Authors:__ Edwin Fine ([`efine@silentcircle.com`](mailto:efine@silentcircle.com)).

This is an HTTP server that simulates the GCM server to some degree.

__WARNING__: Work in progress.

Obviously it does not actually message Android clients, but you can
use it to force known responses for testing without needing a Google
account, or even access to the Internet.

The idea is simple: you tell the simulator what response you want.

Options are:

* HTTP 200 and documented GCM JSON success responses

* HTTP code: 400, 401, 5xx


You tell it what response you want using special headers.


#### <a name="X-GCMSimulator-StatusCode">X-GCMSimulator-StatusCode</a> ####

The desired status code, e.g. 200. If omitted, 200 is the default.


#### <a name="X-GCMSimulator-JSON">X-GCMSimulator-JSON</a> ####

Specific JSON to return, or path of server-side JSON file.  Optional. If provided, `X-GCMSimulator-Results` will be ignored.


#### <a name="X-GCMSimulator-Results">X-GCMSimulator-Results</a> ####

The status code must be 200 (or omitted), failing which this header will be ignored.

The header must be in the format shown below. Examples will follow.

```
Header ::= NameValuePairs OptHeader
OptHeader ::= ';' Header | <empty>
NameValuePairs ::= NameValuePair OptNameValuePairs
OptNameValuePairs ::= ',' NameValuePair | <empty>
NameValuePair ::= Name ':' Value
Name ::= message_id | registration_id | error
```

The numbers returned in the JSON `success`, `failure`, and `error` fields will be set as follows:

```
success = total - failure - canonical_ids
failure = number of error fields
canonical_ids = number of registration_ids
```

If a `registration_id` is provided, `message_id` may be omitted and it will automatically be generated in the response.


#### <a name="X-GCMSimulator-Retry-After">X-GCMSimulator-Retry-After</a> ####

Force the simulator to include a `Retry-After` header set to this number of seconds.


### <a name="TODO">TODO</a> ###

* Complete the simulator - it's still a work in progress.

* Support POST over https.



### <a name="API">API</a> ###

```
POST /your-path HTTP/1.1
Accept: application/json
Content-Type: application/json
<Special Headers>
...
<Regular GCM JSON>
```


#### <a name="Examples_of_requests_using_special_headers">Examples of requests using special headers</a> ####

```
POST /your-path HTTP/1.1
Accept: application/json
Content-Type: application/json
X-GCMSimulator-StatusCode: 400
```

```
POST /your-path HTTP/1.1
Accept: application/json
Content-Type: application/json
X-GCMSimulator-StatusCode: 200
X-GCMSimulator-Results: message_id:1000; registration_id:SomeNewRegId; error:InvalidRegistration
```


### <a name="Configuration">Configuration</a> ###

* `config/vm.args` - edit the environment variables `WEBMACHINE_IP` and
`WEBMACHINE_PORT`. This config will change more often than the other. In this
file, the default IP is set to `0.0.0.0`, and the default port is `5228`.



#### <a name="URL_Path_Configuration">URL Path Configuration</a> ####

Obviously we can't POST to [Google's GCM URL](https://gcm-http.googleapis.com/gcm/send) but we can
at least use the `/gcm/send` part in the simulator, so the default path is
`/gcm/send`. The simulator currently doesn't support https because I need to
figure out what to do about the SSL certs.

If for some reason you want to change this to `/my/special/path/`, check out
dispatch.conf and change `["gcm", "send"]` to `["my","special","path"]`.

Be very careful that the Erlang syntax or webmachine pathspecs are correct. For
technical details on the dispatching mechanism, visit the
[webmachine wiki.](https://github.com/webmachine/webmachine/wiki/Dispatching)
Erlang experience is very helpful when reading this.


### <a name="Running">Running</a> ###

The simulator is meant to become part of a Common Test suite and be started inits own node, or you can
start it manually in an Erlang shell:

```
make run
```


#### <a name="Example_test">Example test</a> ####

```
$ curl -s \
    -H 'Content-Type: application/json' \
    -H 'X-GCMSimulator-StatusCode: 200' \
    -H 'X-GCMSimulator-Results: message_id:0:1358221350928814%921c249af9fd7ecd' \
    --data-binary '{"registration_ids":["1"]}' \
    http://localhost:5228/gcm/send | \
    jq -C .
{
  "multicast_id": 8640044358461744000,
  "success": 1,
  "failure": 0,
  "canonical_ids": 0,
  "results": [
    {
      "message_id": "0:1358221350928814%921c249af9fd7ecd"
    }
  ]
}
```


### <a name="Examples">Examples</a> ###

Example (real) response from Google:

```
{
  "multicast_id": 8640031304644822000,
  "success": 1,
  "failure": 0,
  "canonical_ids": 0,
  "results": [
    {
      "message_id": "0:1358221350928814%921c249af9fd7ecd"
    }
  ]
}
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/SilentCircle/gcm_sim/blob/master/doc/gcm_sim.md" class="module">gcm_sim</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/gcm_sim/blob/master/doc/gcm_sim_app.md" class="module">gcm_sim_app</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/gcm_sim/blob/master/doc/gcm_sim_resource.md" class="module">gcm_sim_resource</a></td></tr>
<tr><td><a href="http://github.com/SilentCircle/gcm_sim/blob/master/doc/gcm_sim_sup.md" class="module">gcm_sim_sup</a></td></tr></table>

