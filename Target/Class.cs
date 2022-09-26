namespace Target;

public class PrivateState
{
    public List<int> Field = new ();
}

public class Class
{
    public int Field1;
    private PrivateState _field2 = new PrivateState();

    public List<int> Method1(int arg)
    {
        if (arg == -234)
        {
            throw new Exception("failed");
        }
        _field2.Field.Add(arg);
        return _field2.Field;
    }

    public static int Method2(int arg)
    {
        if (arg == -234)
        {
            throw new Exception("failed");
        }

        return arg;
    }
}
