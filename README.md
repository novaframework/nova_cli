nova_cli
=====

Usage
-----

Include it in your rebar.config file belonging to your nova application. Then run

```
nova_cli:create_page(my_app, "/route", controller, function).
```

It will create a new controller, view and add it to the route-file.
