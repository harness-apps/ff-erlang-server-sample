# Before you Begin

Harness Feature Flags (FF) is a feature management solution that enables users to change the software’s functionality, without deploying new code. FF uses feature flags to hide code or behaviours without having to ship new versions of the software. A feature flag is like a powerful if statement.

For more information, see https://harness.io/products/feature-flags/

To read more, see https://ngdocs.harness.io/category/vjolt35atg-feature-flags

To sign up, https://app.harness.io/auth/#/signup/

This is a sample app demonstrating Erlang Server SDK integration with CF in an Erlang app.

## Requirements
- Erlang OTP 22 or newer.
- Rebar3


## To use this application, follow the steps as below ##

1) Create a project in Harness with Feature-flags module enabled
2) Create an environment within your project
3) Create a server-side sdk key in your environment **COPY the value from the Admin Console to your clipboard since this value will only be displayed once**
4) Create a boolean feature-flag in the admin console called `harnessappdemodarkmode`
5) Import the Erlang project in an IDE such as [IntelliJ](https://www.jetbrains.com/idea/)

We are using the Erlang SDK as dependency for this sample program

Install dependencies using [Rebar3](https://rebar3.org/docs/basic_usage/#building)
```shell
rebar3 compile
````

In `config/sys.config` add your server-side sdk key from `step 3` above.
```Erlang
  {cfclient, [
    {api_key, {environment_variable, "FF_API_KEY_0"}},
  ]
}
```

Optional: to run [multiple instances of the SDK](https://github.com/harness/ff-erlang-server-sdk/blob/main/README.md#run-multiple-instances-of-the-sdk) for different projects you have on Harness, provide additional config for each additional instance:

```Erlang
# For more complex applications where you need to use multiple Harness projects, you can start up additional
# SDK instances for each project by defining the below config and adding each instance to one of your application supervisors (e.g. the root supervisor)

[
  {harness_project_1_config, [
    {cfclient, [
      {config, [
        {name, instance_name_1}
      ]},
      {api_key, {environment_variable, "FF_API_KEY_2"}}]
    }
  ]
},
  {harness_project_2_config, [
    {cfclient, [
      {config, [
        {name, instance_name_2}
      ]},
      {api_key, {environment_variable, "FF_API_KEY_2"}}]
    }
  ]
  },
  {cfclient, [
    {start_default_instance, true},
    {api_key, {environment_variable, "FF_API_KEY_0"}},
    {config, [
      {config_url, "https://config.ff.harness.io/api/1.0"},
      {events_url, "https://config.ff.harness.io/api/1.0"}
    ]},
    {analytics_push_interval, 60000}
  ]
}]
````
See `src/sampleapp_sup.erl` which adds the above instances to this application's root supervisor. 

Compile the application
```shell
rebar3 compile
```

Boot the application
```shell
rebar3 shell
```

Start the loop to print the flag value every 10 seconds
```shell
sample:
```
