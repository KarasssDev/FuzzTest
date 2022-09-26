using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using Utils;

namespace Allocation;

public class Allocator
{
    private static Type ObjectType;
    private readonly object _toAllocate;

    public Allocator(Type t)
    {
        ObjectType = t;
        _toAllocate = FormatterServices.GetUninitializedObject(ObjectType);
    }

    public object this[string fieldName]
    {
        set
        {
            var field = ObjectType.GetField(fieldName, Reflection.allBindingFlags);
            var property = ObjectType.GetField($"<{fieldName}>k__BackingField", Reflection.allBindingFlags);
            Debug.Assert(field != null || property != null);
            field ??= property;
            if (field != null)
                field.SetValue(_toAllocate, value);
        }
    }

    public object Object => _toAllocate;
}
