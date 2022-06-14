
-include_lib("kernel/include/logger.hrl").

-define(SOH, 1).

-record(group, {
  name  :: atom(),      %% Group name
  fields = []           %% Group fields
}).
