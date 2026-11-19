-- 16 requests per write, as a pipelining client sends them
init = function(args)
  local requests = {}
  for i = 1, 16 do requests[i] = wrk.format(nil, "/") end
  pipelined = table.concat(requests)
end
request = function() return pipelined end
