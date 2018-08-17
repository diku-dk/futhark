public class ValueError : Exception
{
    public ValueError(){}
    public ValueError(string message):base(message){}
    public ValueError(string message, Exception inner):base(message, inner){}
}
