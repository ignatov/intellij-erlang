-export([foo/0]).

foo() ->
  lager:debug("What"), 
  lager:info("What"), 
  lager:notice("What"), 
  lager:warning("What"), 
  lager:error("What"), 
  lager:critical("What"), 
  lager:alert("What"), 
  lager:emergency("What"),
  
  lager:debug("~s", "What"),    
  lager:info("~s", "What"),     
  lager:notice("~s", "What"),   
  lager:warning("~s", "What"),  
  lager:error("~s", "What"),    
  lager:critical("~s", "What"), 
  lager:alert("~s", "What"),
  lager:emergency("~s", "What").