module EmitModel
// http://stevegilham.blogspot.com/2011/05/hello-codedom-building-c-and-f-code.html
// consider instead: http://stackoverflow.com/questions/22879776/using-the-open-source-released-roslyn-to-read-code-file-and-generate-new-code

type System.String with
    member x.Before(delimiter:string) = 
        x.Substring(0,x.IndexOf(delimiter))
    member x.BeforeAnyOf(delimiters:string list) = 
        let index,delimiter = 
            delimiters 
            |> Seq.map ( fun delimiter -> x.IndexOf(delimiter),delimiter)
            |> Seq.filter( fun (index,delimiter) -> index >=0)
            |> Seq.minBy (fun (index, delimiter) -> index)
        x.Substring(0,index)
        
    member x.After(delimiter:string) = 
        x.Substring(x.IndexOf(delimiter) + delimiter.Length)
    member x.SplitLines() =
        x.Split([| "\r\n";"\n"|], System.StringSplitOptions.None);

open Microsoft.CSharp;
open System;
open System.CodeDom;
open System.CodeDom.Compiler;
open System.Collections.Generic;
open System.IO;
open System.Reflection;
open System.Reflection.Emit;
open System.Text;
type IDataRecord = System.Data.IDataRecord
// http://www.codeproject.com/Articles/121568/Dynamic-Type-Using-Reflection-Emit
type MethodGeneratorArguments = 
    | BeforeCreate
    | AfterCreate of Type * ConstructorInfo * MethodBuilder
let generateProperty (tBuilder:TypeBuilder) name propType =
    let fBuilder = tBuilder.DefineField("_"+name, propType, FieldAttributes.Private)
    let pBuilder = tBuilder.DefineProperty(name, PropertyAttributes.HasDefault, propType, Type.EmptyTypes)
    let generateSetter () =
        let mFirstSet = tBuilder.DefineMethod("set_" + pBuilder.Name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig,null, [| fBuilder.FieldType |])
        let firstSetIL = mFirstSet.GetILGenerator()
        firstSetIL.Emit(OpCodes.Ldarg_0) // this
        firstSetIL.Emit(OpCodes.Ldarg_1)
        firstSetIL.Emit(OpCodes.Stfld, fBuilder)
        firstSetIL.Emit(OpCodes.Ret)
        pBuilder.SetSetMethod mFirstSet
        mFirstSet

    let generateGetter () = 
        let mFirstGet = tBuilder.DefineMethod("get_" + pBuilder.Name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, fBuilder.FieldType, Type.EmptyTypes)
        let firstGetIL = mFirstGet.GetILGenerator()
        firstGetIL.Emit(OpCodes.Ldarg_0) // this
        firstGetIL.Emit(OpCodes.Ldfld, fBuilder)
        firstGetIL.Emit(OpCodes.Ret)
        pBuilder.SetGetMethod mFirstGet
        mFirstGet
    name, propType, generateGetter(),generateSetter()
    
//let generateAssemblyCodeDom (compileUnit : CodeCompileUnit) = // not functional, prototype
//    use provider = new CSharpCodeProvider()
//    provider.CompileAssemblyFromDom(new CompilerParameters(Array.empty,"dynamic1.dll"),compileUnit)

let GenerateAssembly assemblyName className props =
    let aName = new Reflection.AssemblyName(assemblyName)
    let appDomain = Threading.Thread.GetDomain()
    let aBuilder = appDomain.DefineDynamicAssembly(aName,Reflection.Emit.AssemblyBuilderAccess.RunAndSave)
    let mBuilder = aBuilder.DefineDynamicModule(aBuilder.GetName().Name + ".dll",aBuilder.GetName().Name + ".dll",true)
    let tBuilder = mBuilder.DefineType(className, TypeAttributes.Public ||| TypeAttributes.Class)
    let readMethodBuilder = tBuilder.DefineMethod("ReadRecord", MethodAttributes.Public, null, [| typeof<IDataRecord> |])
    let pBuilder = readMethodBuilder.DefineParameter(1, ParameterAttributes.None, "record")
    let gen = readMethodBuilder.GetILGenerator()
    //gen.Emit(OpCodes.Ldarg_0)
    let local = gen.DeclareLocal(typeof<obj>) // holds value returned from DataReader.getItem
    let index = gen.DeclareLocal(typeof<bool>)
    let getItemMethod = typeof<IDataRecord>.GetMethod("get_Item",[| typeof<int> |])
//        |> Seq.filter (fun m -> m.Name = "get_Item")
//        |> Seq.skip(1) // skip get_Item by string
//        |> Seq.head
    if getItemMethod = null then failwith "Could not find getItem method";
    let ConstructReadConstructor (m:MethodGeneratorArguments) : Delegate =
        let createMethod t (methodInfo:MethodInfo) (lGen:ILGenerator) =
            let x = lGen.DeclareLocal(t)
            let o = lGen.DeclareLocal(typeof<obj>)
            lGen.Emit(OpCodes.Stloc_0) // x
            lGen.Emit(OpCodes.Ldloc_0) // x
            lGen.Emit(OpCodes.Ldarg_0) // arg reader
            lGen.Emit(OpCodes.Callvirt, methodInfo)
            lGen.Emit(OpCodes.Ldloc_0)
            lGen.Emit(OpCodes.Stloc_1) // x as object (o)
            lGen.Emit(OpCodes.Ldloc_1)
            lGen.Emit(OpCodes.Ret)
        let delegateCaller = 
            match m with
            | BeforeCreate ->
                let loader = tBuilder.DefineMethod("ConstructAndRead", MethodAttributes.Public ||| MethodAttributes.Static, typeof<obj>,[| typeof<System.Data.IDataRecord> |])
                let lGen = loader.GetILGenerator()
                lGen.Emit(OpCodes.Newobj, tBuilder)
                let targetType = tBuilder.AsType()
                createMethod targetType readMethodBuilder lGen
                loader.CreateDelegate
            | AfterCreate(t,ci, methodBuilder) ->
                let loader = new DynamicMethod("ConstructAndRead",typeof<obj>,[| typeof<System.Data.IDataRecord> |])
                let lGen = loader.GetILGenerator()
                lGen.Emit(OpCodes.Newobj, ci)
                let mi = t.GetMethod(readMethodBuilder.Name)
                createMethod t mi lGen
                loader.CreateDelegate
        delegateCaller <| typeof<Func<System.Data.IDataRecord,obj>>
    props 
    |> Seq.map (fun (KeyValue(k,t)) -> generateProperty tBuilder k t)
    //|> ignore
    //|> Seq.take(1)
    |> Seq.iteri (fun i (_,(propType:Type), _, (setter:MethodBuilder)) -> 
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Ldc_I4_S, i) // prepare to read the value from the reader
            gen.Emit(OpCodes.Callvirt, getItemMethod)
            gen.Emit(OpCodes.Stloc_0) //value (local)
            gen.Emit(OpCodes.Ldsfld, typeof<System.DBNull>.GetField("Value"))
            gen.Emit(OpCodes.Ldloc_0) // value (local)
            gen.Emit(OpCodes.Callvirt, typeof<obj>.GetRuntimeMethod("Equals",[| typeof<obj> |]))
            let label = gen.DefineLabel()
            gen.Emit(OpCodes.Stloc_1) // CS$4$0000 (index)
            gen.Emit(OpCodes.Ldloc_1) // CS$4$0000 (index)
(* !!!!!!!!!!!!!!!!!!!!!!! warning branch is going in on dbnull !!!!!!!!!!!!!!!!!!!!!!!!!!! *)
            gen.Emit(OpCodes.Brtrue_S,label) // jump to label +4
            gen.Emit(OpCodes.Ldstr, "Setting " + setter.Name);
            gen.Emit(OpCodes.Call, typeof<System.Diagnostics.Debug>.GetMethod("WriteLine",[|typeof<string>|]))
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldloc_0)
            if propType.IsValueType && propType.Equals(typeof<string>) = false
                then gen.Emit(OpCodes.Unbox_Any,propType)
                else gen.Emit(OpCodes.Castclass,propType)
            gen.Emit(OpCodes.Call,setter)
            gen.Emit(OpCodes.Nop)
            gen.MarkLabel(label)
    )
    gen.Emit(OpCodes.Nop)
    gen.Emit(OpCodes.Ret)
    let declareBeforeCreate = false // declare the constructAndRead method before or after the creation of the type and dll.
    let mutable loader : Delegate = null
    if declareBeforeCreate then
        loader <- ConstructReadConstructor MethodGeneratorArguments.BeforeCreate 
    let generatedType = tBuilder.CreateType()
    let targetFilename = aBuilder.GetName().Name + ".dll" 
    aBuilder.Save(targetFilename)

    if declareBeforeCreate = false then
        let ctor = tBuilder.GetConstructors().[0]
        loader <- ConstructReadConstructor 
            <| AfterCreate(generatedType, ctor,readMethodBuilder) 
    let loadFun = 
        loader :?> Func<System.Data.IDataRecord,obj>
    loadFun
    
    
let GenerateCode (compileUnit: CodeCompileUnit) (provider:CodeDomProvider) =
    let sb = StringBuilder()
    use sw = new StringWriter(sb)
    use tw = new IndentedTextWriter(sw, "    ")
    
    provider.GenerateCodeFromCompileUnit(compileUnit, tw, new CodeGeneratorOptions())
    tw.Close()
    sb.ToString()
    
let GenerateCSharpCode( compileUnit: CodeCompileUnit) =
    use provider = new CSharpCodeProvider()
    GenerateCode compileUnit provider
let requestNs,requestClass = 
//    let fullText:string = System.IO.File.ReadAllText(requestTargetFile)
//    let lines = fullText.SplitLines()
    
    let requestNs =  "FSharpWpfMvvmTemplate.ViewModel"
    let requestClass = "Dyn1"
    requestNs,requestClass
    

