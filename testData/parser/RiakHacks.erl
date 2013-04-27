-define(VNODE_REQ, #riak_vnode_req_v1).
-define(COVERAGE_REQ, #riak_coverage_req_v1).
-define(FOLD_REQ, #riak_core_fold_req_v1).

handle_call({return_vnode, Req=?VNODE_REQ{index=Idx}}) -> Req.
handle_call({return_vnode, Req=?VNODE_REQ{}}) -> Req.
handle_call({return_vnode, Req=?VNODE_REQ.index}) -> Req.