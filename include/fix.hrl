
-include_lib("kernel/include/logger.hrl").

-define(SOH, 1).

-record(fix, {
  msgtype       :: atom(),  %% Message type
  header = nil  :: map(),   %% Optional message header
  fields = #{}  :: map()    %% Message fields
}).

-record(group, {
  name   = nil  :: atom(),  %% Group name
  fields = []   :: list()   %% Group fields
}).
