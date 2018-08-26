private void panic(int exitcode, string str, params Object[] args)
{
    var prog_name = Environment.GetCommandLineArgs()[0];
    Console.Error.WriteLine(String.Format("{0}:", prog_name));
    Console.Error.WriteLine(String.Format(str, args));
    Environment.Exit(exitcode);
}

private void FutharkAssert(bool assertion)
{
    if (!assertion)
    {
        Environment.Exit(1);
    }
}

private void FutharkAssert(bool assertion, string errorMsg)
{
    if (!assertion)
    {
        Console.Error.WriteLine(errorMsg);
        Environment.Exit(1);
    }
}
