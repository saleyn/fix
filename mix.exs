defmodule FIX.MixProject do
  use Mix.Project

  def project do
    [
      app:             :fix,
      version:         "1.0.0",
      elixir:          "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps:            deps(),
      elixirc_paths:   ["src"],
      compilers:       [:nif,:es] ++ Mix.compilers,
    ]
  end

  def application do
    [
      extra_applications: [:logger],
    ]
  end

  defp deps do
    [
      {:parse_trans, "~> 3.3.1", runtime: false},
      {:etran,       "~> 0.3",   runtime: false},
      {:util,        "~> 1.0"},
    ]
  end
end

defmodule Mix.Tasks.Compile.Nif do
  def run(_args) do
    {result, _errcode} = System.cmd("make", ["nif"])
    IO.binwrite(result)
  end
end

defmodule Mix.Tasks.Compile.Es do
  def run(_args) do
    val  = System.get_env("FIXDUMP_ENV",      "")
    escr = System.get_env("ESCRIPT",   "escript")
    body = File.read!("src/fixdump.es.src")
        |> String.replace("{{ESCRIPT}}",    escr)
        |> String.replace("{{FIXDUMP_ENV}}", val)
    :ok  = File.write!("priv/fixdump.es",   body)
    :ok  = File.chmod!("priv/fixdump.es",  0o755)
    IO.puts("Copied src/fixdump.es.src -> priv/fixdump.es")
  end
end
