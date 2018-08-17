public class TypeError : Exception
{
    public TypeError(){}
    public TypeError(string message):base(message){}
    public TypeError(string message, Exception inner):base(message, inner){}
}

public class ValueError : Exception
{
    public ValueError(){}
    public ValueError(string message):base(message){}
    public ValueError(string message, Exception inner):base(message, inner){}
}
