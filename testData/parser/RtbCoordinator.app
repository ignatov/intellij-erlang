{application, rtb_coordinator, [
    {description, "description"},
    {modules, []},
    {registered, []},
    {applications, [
        kernel,
        stdlib,
        crypto,
        public_key,
        ssl,
        cowboy
    ]},
    {mod, {rtb_coordinator_app, []}},
    {env, [
        {orders_db, "/var/lib/rtb/coordinator/orders"},
        {budget_db, "/var/lib/rtb/coordinator/orders"},
        {dispatch, [
            {'_', [
                {[<<"orders">>], insertion_order_handler, []},
                {[<<"orders">>, data], insertion_order_handler, []},
%% set index.html as root directory index
                {[], cowboy_http_static, [
                    {directory, {priv_dir, rtb_coordinator, [<<"public">>]}},
                    {file, <<"index.html">>},
                    {mimetypes, [
                        {<<".html">>, [<<"text/html">>]}]}
                ]},
%% serve js, css and images
                {['...'], cowboy_http_static, [
                    {directory, {priv_dir, rtb_coordinator, [<<"public">>]}},
                    {mimetypes, [
                        {<<".js">>, [<<"application/javascript">>]},
                        {<<".png">>, [<<"image/png">>]},
                        {<<".css">>, [<<"text/css">>]}
                    ]}
                ]}
            ]}
        ]}
    ]}
]}.
