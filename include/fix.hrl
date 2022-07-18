-include_lib("kernel/include/logger.hrl").

-define(SOH, 1).

-record(fix, {
  msgtype       :: atom(),    %% Message type
  header = #{}  :: map(),     %% Optional message header
  fields = #{}  :: map()      %% Message fields
}).

-record(meta, {
  type          :: field|group,
  required      :: boolean(),
  order         :: integer(), %% Field order in the spec
  content = nil :: map()|nil  %% A group contains a list of fields
}).
