namespace Utils

open System
open System.Collections.Generic
open System.Reflection

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // ------------------------------- Assemblies -------------------------------

    let loadAssembly (assemblyName : string) =
        let assemblies = AppDomain.CurrentDomain.GetAssemblies()
        let dynamicAssemblies = assemblies |> Array.filter (fun a -> a.IsDynamic)
        let dynamicOption = dynamicAssemblies |> Array.tryFind (fun a -> a.FullName.Contains(assemblyName))
        match dynamicOption with
        | Some a -> a
        | None -> Assembly.Load(assemblyName)

    // --------------------------- Metadata Resolving ---------------------------

    let resolveModule (assemblyName : string) (moduleName : string) =
        let assembly =
            match AppDomain.CurrentDomain.GetAssemblies() |> Array.tryFindBack (fun assembly -> assembly.FullName = assemblyName) with
            | Some assembly -> assembly
            | None ->
                try
                    Assembly.Load(assemblyName)
                with _ ->
                    Assembly.LoadFile(moduleName)
        assembly.Modules |> Seq.find (fun m -> m.FullyQualifiedName = moduleName)

    let resolveMethodBase (assemblyName : string) (moduleName : string) (token : int32) =
        let m = resolveModule assemblyName moduleName
        m.ResolveMethod(token)

    let private retrieveMethodsGenerics (method : MethodBase) =
        match method with
        | :? MethodInfo as mi -> mi.GetGenericArguments()
        | :? ConstructorInfo -> null
        | _ -> failwith "not implemented"

    let resolveModuleFromAssembly (assembly : Assembly) (moduleName : string) =
        assembly.GetModule moduleName

    let resolveTypeFromModule (m : Module) typeToken =
        m.ResolveType(typeToken, null, null)

    let resolveField (method : MethodBase) fieldToken =
        let methodsGenerics = retrieveMethodsGenerics method
        let typGenerics = method.DeclaringType.GetGenericArguments()
        method.Module.ResolveField(fieldToken, typGenerics, methodsGenerics)

    let resolveType (method : MethodBase) typeToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveType(typeToken, typGenerics, methodGenerics)

    let resolveMethod (method : MethodBase) methodToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMethod(methodToken, typGenerics, methodGenerics)

    let resolveToken (method : MethodBase) token =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMember(token, typGenerics, methodGenerics)

    // --------------------------------- Methods --------------------------------


    let isSubtypeOrEqual (t1 : Type) (t2 : Type) = t2.IsAssignableFrom(t1)
    // TODO: what if return type is generic?
    let getMethodReturnType : MethodBase -> Type = function
        | :? ConstructorInfo -> typeof<Void>
        | :? MethodInfo as m -> m.ReturnType
        | _ -> failwith "unknown MethodBase"

    let hasNonVoidResult m = (getMethodReturnType m).FullName <> typeof<Void>.FullName

    let hasThis (m : MethodBase) = m.CallingConvention.HasFlag(CallingConventions.HasThis)

    let getFullTypeName (typ : Type) = typ.ToString()

    let getFullMethodName (methodBase : MethodBase) =
        let returnType = getMethodReturnType methodBase |> getFullTypeName
        let declaringType = getFullTypeName methodBase.DeclaringType
        let parameters =
            methodBase.GetParameters()
            |> Seq.map (fun param -> getFullTypeName param.ParameterType)
            |> if methodBase.IsStatic then id else (fun x -> seq {yield! x
                                                                  "this"}
            )
            |> fun x -> String.Join( ",", Seq.toArray x)
//        let typeParams =
//            if not methodBase.IsGenericMethod then ""
//            else methodBase.GetGenericArguments() |> Seq.map getFullTypeName |> join ", " |> sprintf "[%s]"
        sprintf "%s %s.%s(%s)" returnType declaringType methodBase.Name parameters

    let isArrayConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsArray

    let isDelegateConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate>

    let isDelegate (methodBase : MethodBase) =
        isSubtypeOrEqual methodBase.DeclaringType typedefof<Delegate> && methodBase.Name = "Invoke"

    let isGenericOrDeclaredInGenericType (methodBase : MethodBase) =
        methodBase.IsGenericMethod || methodBase.DeclaringType.IsGenericType

    let isStaticConstructor (m : MethodBase) =
        m.IsStatic && m.Name = ".cctor"

    let isExternalMethod (methodBase : MethodBase) =
        let isInternalCall = methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall
        let isPInvokeImpl = methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)
        int isInternalCall <> 0 || isPInvokeImpl

    let getAllMethods (t : Type) = t.GetMethods(allBindingFlags)

    let getMethodDescriptor (m : MethodBase) =
        let declaringType = m.DeclaringType
        let declaringTypeVars =
            if declaringType.IsGenericType then declaringType.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        let methodVars =
            if m.IsGenericMethod then m.GetGenericArguments() |> Array.map (fun t -> t.TypeHandle.Value)
            else [||]
        m.MethodHandle.Value, declaringTypeVars, methodVars, m.ReflectedType.TypeHandle.Value

    let compareMethods (m1 : MethodBase) (m2 : MethodBase) =
        compare (getMethodDescriptor m1) (getMethodDescriptor m2)

    // ----------------------------------- Creating objects ----------------------------------

    let rec isReferenceTypeParameter (t : Type) =
        let checkAttribute (t : Type) =
            t.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint <> GenericParameterAttributes.None
        let isSimpleReferenceConstraint (t : Type) = t.IsClass && t <> typeof<ValueType>
        let isReferenceConstraint (c : Type) = if c.IsGenericParameter then isReferenceTypeParameter c else isSimpleReferenceConstraint c
        checkAttribute t || t.GetGenericParameterConstraints() |> Array.exists isReferenceConstraint

    let isNullable = function
        | (t : Type) when t.IsGenericParameter ->
            if isReferenceTypeParameter t then false
            else failwith "Can't determine if %O is a nullable type or not!" t
        | t -> Nullable.GetUnderlyingType(t) <> null
    let defaultOf (t : Type) =
        if t.IsValueType && not (isNullable t) && not t.ContainsGenericParameters
            then Activator.CreateInstance t
            else null

    let createObject (t : Type) =
        match t with
        | _ when t = typeof<String> -> String.Empty :> obj
        | _ when isNullable t -> null
        | _ when t.IsArray -> Array.CreateInstance(typeof<obj>, 1)
        | _ -> System.Runtime.Serialization.FormatterServices.GetUninitializedObject t
