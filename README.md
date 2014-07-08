erlang游戏服务器使用的cache模块
=====

初衷
--------
查看其他玩家数据（热点玩家的数据想活跃在内存，能加速访问，冷的玩家被访问之后，过了TTL之后，会自动从内存清除）或者binary编译出来的关键字MP要找个共享地方存，从而有这样的一个模块，本身就一个项目在用，后来多个项目一起用，复制粘帖太麻烦了，就独立出来，欢迎fork。

功能
--------
1. time to live 定时清理，ets_cache_manager负责，默认是10分钟，可以通过application的env配置
2. 并发读/写，从而会有资源竞争
3. 自动从mnesia读

安装
--------

在你的`rebar.config`添加:

    {reloader, ".*", {git, "https://github.com/roowe/ets_cache", "master"}}

之后执行 `rebar get-deps`接着 `rebar compile`.

用法
--------
```erlang
1> application:start(ets_cache).
ok
2> ets_cache:get(test_key).
[]
3> ets_cache:set(test_key, 1).
ok
4> ets_cache:get(test_key).
```

其他可以看test/ets_cache_tests.erl

单元测试
--------
`rebar eunit`
