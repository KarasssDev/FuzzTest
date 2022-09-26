open System
open FsCheck
open System.Reflection
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open NUnit.Framework
open Allocation
open Utils



module Testing =


    type OptionBuilder() =
        member x.Bind(v,f) = Option.bind f v
        member x.Return v = Some v
        member x.ReturnFrom o = o
        member x.Zero () = None

    let opt = OptionBuilder()



    let testMethod assemblyName moduleName className methodName =

        let getMethod assemblyName className methodName =
            let assembly = Reflection.loadAssembly assemblyName
            let targetModule = Reflection.resolveModuleFromAssembly assembly moduleName

            let methodToken = opt {
                let classes = targetModule.GetTypes()
                let! targetClass = Seq.tryFind (fun (c: Type) -> c.Name = className) classes
                let methods = targetClass.GetMethods()
                let! targetMethod = Seq.tryFind (fun (m: MethodInfo) -> m.Name = methodName) methods

                return targetMethod.MetadataToken, targetMethod
            }
            match methodToken with
            | Some (n, info) -> targetModule.ResolveMethod(n), info
            | None -> failwith $"Method{methodName} not found"

        let getMethodArgsTypes (method: MethodBase) =
            let args = method.GetParameters()
            Array.filter (fun (i: ParameterInfo) -> not i.IsRetval) args
            |> Array.map (fun (i: ParameterInfo) -> i.ParameterType)

        let methodBase, methodInfo = getMethod assemblyName className methodName

        if not methodBase.IsStatic then
            failwith "testing non static method not implemented"

        let fakeProperty (arg: int) = // здесь проблема с аргументами
            methodBase.Invoke(null, [|arg|]) |> ignore
            true

        Check.Quick fakeProperty

    [<EntryPoint>]
    let main _ =
        testMethod "Target" "Target.dll" "Class" "Method2"
        0

