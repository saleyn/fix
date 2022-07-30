##------------------------------------------------------------------------------
## Modules for representing Elixir errors
##------------------------------------------------------------------------------

defmodule FIX.EncodeError do
  @moduledoc "FIX exception indicating an encoding error"
  @dialyzer :no_undefined_callbacks

  defexception [:tag, :message, :src]

  def message(e), do: e.message
end

defmodule FIX.DecodeError do
  @moduledoc "FIX exception indicating an decoding error"
  @dialyzer :no_undefined_callbacks

  defexception [:tag, :message, :pos, :reason, :src]

  def message(e), do: e.message
end
