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
      compilers:       [:priv] ++ Mix.compilers,
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
  end
end

defmodule Mix.Tasks.Compile.Priv do
  use Mix.Task.Compiler

  @fixdump "priv/fixdump.es"

  @impl true
  def run(_args) do
    outdir = Keyword.get(Mix.Project.config(), :app_path, File.cwd!)

    {result, _errcode} = System.cmd("make", ["nif"], [env: [{"REBAR_BARE_COMPILER_OUTPUT_DIR", outdir}]])
    IO.binwrite(result)

    val  = System.get_env("FIXDUMP_ENV",      "")
    escr = System.get_env("ESCRIPT",   "escript")
    body = File.read!("src/fixdump.es.src")
        |> String.replace("{{ESCRIPT}}",    escr)
        |> String.replace("{{FIXDUMP_ENV}}", val)
    :ok  = File.write!(@fixdump,  body)
    :ok  = File.chmod!(@fixdump, 0o755)
    IO.puts("Copied src/fixdump.es.src -> " <> @fixdump)

    if outdir != File.cwd! do
      :ok = File.cp!(@fixdump, outdir <> @fixdump)
      IO.puts("Copied #{@fixdump}, #{outdir <> @fixdump}")
    end

    :ok
  end

  @impl true
  def clean() do
    :ok == File.rm(@fixdump) && IO.puts("Deleted " <> @fixdump)
    files = File.rm_rf!("priv")
    files != [] && IO.puts("Deleted #{inspect(files)}")

    outdir = Keyword.get(Mix.Project.config(), :app_path)
    outdir && Path.join(outdir, "priv") |> File.rm_rf! |> then(& (&1 != [] && IO.puts("Deleted #{inspect(&1)}")))
  end
end
