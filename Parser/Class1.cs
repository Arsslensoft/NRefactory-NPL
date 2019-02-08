using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ICSharpCode.NRefactory.Rava.Parser
{
    class ConversionVisitor : StructuralVisitor
    {
        SyntaxTree unit = new SyntaxTree();
        internal bool convertTypeSystemMode;

        public SyntaxTree Unit
        {
            get
            {
                return unit;
            }
            set
            {
                unit = value;
            }
        }

        public LocationsBag LocationsBag
        {
            get;
            private set;
        }

        public ConversionVisitor(bool convertTypeSystemMode, LocationsBag locationsBag)
        {
            this.convertTypeSystemMode = convertTypeSystemMode;
            this.LocationsBag = locationsBag;
        }

        public static TextLocation Convert(Location loc)
        {
            return new TextLocation(loc.Row, loc.Column);
        }

  

        #region Global

        readonly Stack<NamespaceDeclaration> namespaceStack = new Stack<NamespaceDeclaration>();

        void AddTypeArguments(ATypeNameExpression texpr, AstType result)
        {
            var unbound = texpr.TypeArguments as UnboundTypeArguments;
            if (unbound != null)
            {
                TextLocation ll = Convert(texpr.Location);
                result.AddChild(new CSharpTokenNode(ll, Roles.LChevron), Roles.LChevron);
                ll = new TextLocation(ll.Line, ll.Column + 1);
                for (int j = 0; j < unbound.Count; j++)
                {
                    result.AddChild(new SimpleType(), Roles.TypeArgument);
                    result.AddChild(new CSharpTokenNode(ll, Roles.LChevron), Roles.Comma);
                    ll = new TextLocation(ll.Line, ll.Column + 1);
                }
                result.AddChild(new CSharpTokenNode(ll, Roles.RChevron), Roles.RChevron);
                /*
                var loc2 = LocationsBag.GetLocations(texpr.TypeArguments);
                if (loc2 == null)
                    return;
                int j = 0;
                if (j < loc2.Count)
                    result.AddChild(new CSharpTokenNode(Convert(loc2 [j++]), Roles.LChevron), Roles.LChevron);
                while (j < loc2.Count - 1) {
                    result.AddChild (new SimpleType (), Roles.TypeArgument);
                    result.AddChild(new CSharpTokenNode(Convert(loc2 [j++]), Roles.LChevron), Roles.Comma);
                }
                if (j < loc2.Count) {
                    result.AddChild (new SimpleType (), Roles.TypeArgument);
                    result.AddChild(new CSharpTokenNode(Convert(loc2 [j++]), Roles.RChevron), Roles.RChevron);
                }*/
                return;
            }
            if (texpr.TypeArguments == null || texpr.TypeArguments.Args == null)
                return;
            var loc = LocationsBag.GetLocations(texpr.TypeArguments);
            if (loc != null && loc.Count >= 2)
                result.AddChild(new CSharpTokenNode(Convert(loc[loc.Count - 2]), Roles.LChevron), Roles.LChevron);
            int i = 0;
            foreach (var arg in texpr.TypeArguments.Args)
            {
                result.AddChild(ConvertToType(arg), Roles.TypeArgument);
                if (loc != null && i < loc.Count - 2)
                    result.AddChild(new CSharpTokenNode(Convert(loc[i++]), Roles.Comma), Roles.Comma);
            }
            if (loc != null && loc.Count >= 2)
                result.AddChild(new CSharpTokenNode(Convert(loc[loc.Count - 1]), Roles.RChevron), Roles.RChevron);
        }

        static AstType ConvertToType(TypeParameter spec)
        {
            AstType result;
            result = new SimpleType { IdentifierToken = Identifier.Create(spec.Name, Convert(spec.Location)) };
            return result;
        }

        AstType ConvertToType(MemberName memberName)
        {
            AstType result;
            if (memberName.Left != null)
            {
                result = new MemberType();
                result.AddChild(ConvertToType(memberName.Left), MemberType.TargetRole);
                var loc = LocationsBag.GetLocations(memberName);
                if (loc != null)
                    result.AddChild(new CSharpTokenNode(Convert(loc[0]), Roles.Dot), Roles.Dot);
                result.AddChild(Identifier.Create(memberName.Name, Convert(memberName.Location)), Roles.Identifier);
            }
            else
            {
                result = new SimpleType { IdentifierToken = Identifier.Create(memberName.Name, Convert(memberName.Location)) };
            }
            if (memberName.TypeParameters != null)
            {
                var chevronLocs = LocationsBag.GetLocations(memberName.TypeParameters);
                if (chevronLocs != null)
                    result.AddChild(new CSharpTokenNode(Convert(chevronLocs[chevronLocs.Count - 2]), Roles.LChevron), Roles.LChevron);
                for (int i = 0; i < memberName.TypeParameters.Count; i++)
                {
                    var param = memberName.TypeParameters[i];
                    result.AddChild(new SimpleType(Identifier.Create(param.Name, Convert(param.Location))), Roles.TypeArgument);
                    if (chevronLocs != null && i < chevronLocs.Count - 2)
                        result.AddChild(new CSharpTokenNode(Convert(chevronLocs[i]), Roles.Comma), Roles.Comma);
                }
                if (chevronLocs != null)
                    result.AddChild(new CSharpTokenNode(Convert(chevronLocs[chevronLocs.Count - 1]), Roles.RChevron), Roles.RChevron);
            }
            return result;
        }

        public override void Visit(NamespaceContainer ns)
        {
            NamespaceDeclaration nDecl = null;
            var loc = LocationsBag.GetLocations(ns);
            // <invalid> is caused by the parser - see Bug 12383 - [AST] Non existing namespaces generated
            if (ns.NS != null && !string.IsNullOrEmpty(ns.NS.Name) && !ns.NS.Name.EndsWith("<invalid>", StringComparison.Ordinal))
            {
                nDecl = new NamespaceDeclaration();
                if (loc != null)
                {
                    nDecl.AddChild(new CSharpTokenNode(Convert(loc[0]), Roles.NamespaceKeyword), Roles.NamespaceKeyword);
                }
                nDecl.AddChild(ConvertNamespaceName(ns.RealMemberName), NamespaceDeclaration.NamespaceNameRole);
                if (loc != null && loc.Count > 1)
                {
                    nDecl.AddChild(new CSharpTokenNode(Convert(loc[1]), Roles.LBrace), Roles.LBrace);
                }

                AddToNamespace(nDecl);
                namespaceStack.Push(nDecl);
            }

            if (ns.Usings != null)
            {
                foreach (var us in ns.Usings)
                {
                    us.Accept(this);
                }
            }

            if (ns.Containers != null)
            {
                foreach (var container in ns.Containers)
                {
                    container.Accept(this);
                }
            }

            if (nDecl != null)
            {
                AddAttributeSection(nDecl, ns.UnattachedAttributes, EntityDeclaration.UnattachedAttributeRole);
                if (loc != null && loc.Count > 2)
                    nDecl.AddChild(new CSharpTokenNode(Convert(loc[2]), Roles.RBrace), Roles.RBrace);
                if (loc != null && loc.Count > 3)
                    nDecl.AddChild(new CSharpTokenNode(Convert(loc[3]), Roles.Semicolon), Roles.Semicolon);

                namespaceStack.Pop();
            }
        }
        //			public override void Visit (UsingsBag.Namespace nspace)
        //			{
        //
        //
        //				VisitNamespaceUsings (nspace);
        //				VisitNamespaceBody (nspace);
        //
        //			}
        //
        AstType ConvertNamespaceName(MemberName memberName)
        {
            // HACK for a parser 'bug' - sometimes it generates "<invalid>" identifiers in namespace names (on certain bugs in the input file)
            if (memberName.Name == "<invalid>")
                return AstType.Null;
            return ConvertToType(memberName);
        }


       
     

        AstType ConvertImport(MemberName memberName)
        {
            if (memberName.Left != null)
            {
                // left.name
                var t = new MemberType();
                //					t.IsDoubleColon = memberName.IsDoubleColon;
                t.AddChild(ConvertImport(memberName.Left), MemberType.TargetRole);
                var loc = LocationsBag.GetLocations(memberName);
                if (loc != null)
                    t.AddChild(new CSharpTokenNode(Convert(loc[0]), Roles.Dot), Roles.Dot);

                t.AddChild(Identifier.Create(memberName.Name, Convert(memberName.Location)), Roles.Identifier);
                AddTypeArguments(t, memberName);
                return t;
            }
            else
            {
                var t = new SimpleType();
                t.AddChild(Identifier.Create(memberName.Name, Convert(memberName.Location)), Roles.Identifier);
                AddTypeArguments(t, memberName);
                return t;
            }
        }

        public override void Visit(MemberCore member)
        {
            Console.WriteLine("Unknown member:");
            Console.WriteLine(member.GetType() + "-> Member {0}", member.GetSignatureForError());
        }

        readonly Stack<TypeDeclaration> typeStack = new Stack<TypeDeclaration>();



    

     
 

    

   
        #endregion

        #region Type members




   

        static readonly Dictionary<ICSharpCode.NRefactory.MonoRava.Modifiers, Modifiers> modifierTable = new Dictionary<ICSharpCode.NRefactory.MonoRava.Modifiers, Modifiers>();
        static readonly string[] keywordTable;

        static ConversionVisitor()
        {
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.NEW] = Modifiers.New;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.PUBLIC] = Modifiers.Public;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.PROTECTED] = Modifiers.Protected;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.PRIVATE] = Modifiers.Private;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.INTERNAL] = Modifiers.Internal;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.ABSTRACT] = Modifiers.Abstract;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.VIRTUAL] = Modifiers.Virtual;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.SEALED] = Modifiers.Sealed;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.STATIC] = Modifiers.Static;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.OVERRIDE] = Modifiers.Override;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.READONLY] = Modifiers.Readonly;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.PARTIAL] = Modifiers.Partial;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.EXTERN] = Modifiers.Extern;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.VOLATILE] = Modifiers.Volatile;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.UNSAFE] = Modifiers.Unsafe;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.ASYNC] = Modifiers.Async;
            modifierTable[ICSharpCode.NRefactory.MonoRava.Modifiers.SYNC] = Modifiers.Sync;
            keywordTable = new string[255];
            for (int i = 0; i < keywordTable.Length; i++)
                keywordTable[i] = "unknown";

            keywordTable[(int)BuiltinTypeSpec.Type.Other] = "sub";
            keywordTable[(int)BuiltinTypeSpec.Type.String] = "string";
            keywordTable[(int)BuiltinTypeSpec.Type.Int] = "int";
            keywordTable[(int)BuiltinTypeSpec.Type.Object] = "object";
            keywordTable[(int)BuiltinTypeSpec.Type.Float] = "float";
            keywordTable[(int)BuiltinTypeSpec.Type.Double] = "double";
            keywordTable[(int)BuiltinTypeSpec.Type.Long] = "long";
            keywordTable[(int)BuiltinTypeSpec.Type.Byte] = "byte";
            keywordTable[(int)BuiltinTypeSpec.Type.UInt] = "uint";
            keywordTable[(int)BuiltinTypeSpec.Type.ULong] = "ulong";
            keywordTable[(int)BuiltinTypeSpec.Type.Short] = "short";
            keywordTable[(int)BuiltinTypeSpec.Type.UShort] = "ushort";
            keywordTable[(int)BuiltinTypeSpec.Type.SByte] = "sbyte";
            keywordTable[(int)BuiltinTypeSpec.Type.Decimal] = "decimal";
            keywordTable[(int)BuiltinTypeSpec.Type.Complex] = "complex";
            keywordTable[(int)BuiltinTypeSpec.Type.Char] = "char";
            keywordTable[(int)BuiltinTypeSpec.Type.Bool] = "bool";


            keywordTable[(int)BuiltinTypeSpec.Type.Quad] = "quad";
            keywordTable[(int)BuiltinTypeSpec.Type.VLong] = "vlong";
            keywordTable[(int)BuiltinTypeSpec.Type.Date] = "date";
            keywordTable[(int)BuiltinTypeSpec.Type.Time] = "time";
            keywordTable[(int)BuiltinTypeSpec.Type.UIntPtr] = "upointer";
            keywordTable[(int)BuiltinTypeSpec.Type.IntPtr] = "pointer";
        }


        void AddExplicitInterface(AstNode parent, MemberName memberName)
        {
            if (memberName == null || memberName.ExplicitInterface == null)
                return;

            parent.AddChild(ConvertToType(memberName.ExplicitInterface), EntityDeclaration.PrivateImplementationTypeRole);
            var privateImplTypeLoc = LocationsBag.GetLocations(memberName.ExplicitInterface);
            if (privateImplTypeLoc != null)
                parent.AddChild(new CSharpTokenNode(Convert(privateImplTypeLoc[0]), Roles.Dot), Roles.Dot);
        }


        #endregion

        #region Statements

        public override object Visit(ICSharpCode.NRefactory.MonoRava.Statement stmt)
        {
            Console.WriteLine("unknown statement:" + stmt);
            return null;
        }





        void AddStatementOrList(ForStatement forStatement, ICSharpCode.NRefactory.MonoRava.Statement init, Role<Statement> role)
        {
            if (init == null)
                return;
            var stmtList = init as StatementList;
            if (stmtList != null)
            {
                foreach (var stmt in stmtList.Statements)
                {
                    forStatement.AddChild((Statement)stmt.Accept(this), role);
                }
            }
            else if (init is ICSharpCode.NRefactory.MonoRava.EmptyStatement)
            {

            }
            else
            {
                forStatement.AddChild((Statement)init.Accept(this), role);
            }
        }



        public override object Visit(StatementErrorExpression errorStatement)
        {
            var result = new ExpressionStatement();
            var expr = errorStatement.Expr.Accept(this) as Expression;
            if (expr != null)
                result.AddChild(expr, Roles.Expression);
            var location = LocationsBag.GetLocations(errorStatement);
            if (location != null)
                result.AddChild(new CSharpTokenNode(Convert(location[0]), Roles.Semicolon), Roles.Semicolon);
            return result;
        }


     

  



  

   

        public static bool IsLower(Location left, Location right)
        {
            return left.Row < right.Row || left.Row == right.Row && left.Column < right.Column;
        }

        public UsingStatement CreateUsingStatement(Block blockStatement)
        {
            var usingResult = new UsingStatement();
            ICSharpCode.NRefactory.MonoRava.Statement cur = blockStatement.Statements[0];
            var u = cur as Using;
            if (u != null)
            {
                usingResult.AddChild(new CSharpTokenNode(Convert(u.loc), UsingStatement.UsingKeywordRole), UsingStatement.UsingKeywordRole);
                usingResult.AddChild(new CSharpTokenNode(Convert(blockStatement.StartLocation), Roles.LPar), Roles.LPar);
                if (u.Variables != null)
                {
                    var initializer = new VariableInitializer
                    {
                        NameToken = Identifier.Create(u.Variables.Variable.Name, Convert(u.Variables.Variable.Location)),
                    };

                    var loc = LocationsBag.GetLocations(u.Variables);
                    if (loc != null)
                        initializer.AddChild(new CSharpTokenNode(Convert(loc[0]), Roles.Assign), Roles.Assign);
                    if (u.Variables.Initializer != null)
                        initializer.Initializer = u.Variables.Initializer.Accept(this) as Expression;


                    var varDec = new VariableDeclarationStatement
                    {
                        Type = ConvertToType(u.Variables.TypeExpression),
                        Variables = { initializer }
                    };

                    if (u.Variables.Declarators != null)
                    {
                        foreach (var decl in u.Variables.Declarators)
                        {
                            var declLoc = LocationsBag.GetLocations(decl);
                            var init = new VariableInitializer();
                            if (declLoc != null && declLoc.Count > 0)
                                varDec.AddChild(new CSharpTokenNode(Convert(declLoc[0]), Roles.Comma), Roles.Comma);
                            init.AddChild(Identifier.Create(decl.Variable.Name, Convert(decl.Variable.Location)), Roles.Identifier);
                            if (decl.Initializer != null)
                            {
                                if (declLoc != null && declLoc.Count > 1)
                                    init.AddChild(new CSharpTokenNode(Convert(declLoc[1]), Roles.Assign), Roles.Assign);
                                init.AddChild((Expression)decl.Initializer.Accept(this), Roles.Expression);
                            }
                            varDec.AddChild(init, Roles.Variable);
                        }
                    }
                    usingResult.AddChild(varDec, UsingStatement.ResourceAcquisitionRole);
                }
                cur = u.Statement;
                usingResult.AddChild(new CSharpTokenNode(Convert(blockStatement.EndLocation), Roles.RPar), Roles.RPar);
                if (cur != null)
                    usingResult.AddChild((Statement)cur.Accept(this), Roles.EmbeddedStatement);
            }
            return usingResult;
        }

      
  
  
   
       
   

     


        #endregion

        #region Expression

    

        public override object Visit(MemberAccess memberAccess)
        {
            Expression result;
            var ind = memberAccess.LeftExpression as Indirection;
            if (ind != null)
            {
                result = new PointerReferenceExpression();
                result.AddChild((Expression)ind.Expr.Accept(this), Roles.TargetExpression);
                result.AddChild(new CSharpTokenNode(Convert(ind.Location), PointerReferenceExpression.ArrowRole), PointerReferenceExpression.ArrowRole);
            }
            else
            {
                result = new MemberReferenceExpression();
                if (memberAccess.LeftExpression != null)
                {
                    var leftExpr = memberAccess.LeftExpression.Accept(this);
                    result.AddChild((Expression)leftExpr, Roles.TargetExpression);
                }
                var loc = LocationsBag.GetLocations(memberAccess);

                if (loc != null)
                {
                    result.AddChild(new CSharpTokenNode(Convert(loc[0]), Roles.Dot), Roles.Dot);
                }
            }

            result.AddChild(Identifier.Create(memberAccess.Name, Convert(memberAccess.Location)), Roles.Identifier);

            AddTypeArguments(result, memberAccess);
            return result;
        }

   

        public override object Visit(Constant constant)
        {
            if (constant.GetValue() == null)
                return new NullReferenceExpression(Convert(constant.Location));
            string literalValue;
            var literalConstant = constant as ILiteralConstant;
            literalValue = literalConstant != null ? new string(literalConstant.ParsedValue) : constant.GetValueAsLiteral();
            object val = constant.GetValue();
            if (val is bool)
                literalValue = (bool)val ? "true" : "false";
            var result = new PrimitiveExpression(val, Convert(constant.Location), literalValue);
            return result;
        }

  

        public override object Visit(BooleanExpression booleanExpression)
        {
            return booleanExpression.Expr.Accept(this);
        }
   

        public override object Visit(UnaryMutator unaryMutatorExpression)
        {
            var result = new UnaryOperatorExpression();
            if (unaryMutatorExpression.Expr == null)
                return result;
            var expression = (Expression)unaryMutatorExpression.Expr.Accept(this);
            switch (unaryMutatorExpression.UnaryMutatorMode)
            {
                case UnaryMutator.Mode.PostDecrement:
                    result.Operator = UnaryOperatorType.PostDecrement;
                    result.AddChild(expression, Roles.Expression);
                    result.AddChild(new CSharpTokenNode(Convert(unaryMutatorExpression.Location), UnaryOperatorExpression.DecrementRole), UnaryOperatorExpression.DecrementRole);
                    break;
                case UnaryMutator.Mode.PostIncrement:
                    result.Operator = UnaryOperatorType.PostIncrement;
                    result.AddChild(expression, Roles.Expression);
                    result.AddChild(new CSharpTokenNode(Convert(unaryMutatorExpression.Location), UnaryOperatorExpression.IncrementRole), UnaryOperatorExpression.IncrementRole);
                    break;

                case UnaryMutator.Mode.PreIncrement:
                    result.Operator = UnaryOperatorType.Increment;
                    result.AddChild(new CSharpTokenNode(Convert(unaryMutatorExpression.Location), UnaryOperatorExpression.IncrementRole), UnaryOperatorExpression.IncrementRole);
                    result.AddChild(expression, Roles.Expression);
                    break;
                case UnaryMutator.Mode.PreDecrement:
                    result.Operator = UnaryOperatorType.Decrement;
                    result.AddChild(new CSharpTokenNode(Convert(unaryMutatorExpression.Location), UnaryOperatorExpression.DecrementRole), UnaryOperatorExpression.DecrementRole);
                    result.AddChild(expression, Roles.Expression);
                    break;
            }

            return result;
        }

        public override object Visit(Indirection indirectionExpression)
        {
            var result = new UnaryOperatorExpression();
            result.Operator = UnaryOperatorType.Dereference;
            result.AddChild(new CSharpTokenNode(Convert(indirectionExpression.Location), UnaryOperatorExpression.DereferenceRole), UnaryOperatorExpression.DereferenceRole);
            if (indirectionExpression.Expr != null)
                result.AddChild((Expression)indirectionExpression.Expr.Accept(this), Roles.Expression);
            return result;
        }

    

 
    

        public override object Visit(ComposedCast composedCast)
        {
            var result = new ComposedType();
            result.AddChild(ConvertToType(composedCast.Left), Roles.Type);

            var spec = composedCast.Spec;
            while (spec != null)
            {
                if (spec.IsNullable)
                {
                    result.AddChild(new CSharpTokenNode(Convert(spec.Location), ComposedType.NullableRole), ComposedType.NullableRole);
                }
                else if (spec.IsPointer)
                {
                    result.AddChild(new CSharpTokenNode(Convert(spec.Location), ComposedType.PointerRole), ComposedType.PointerRole);
                }
                else
                {
                    var aSpec = new ArraySpecifier();
                    aSpec.AddChild(new CSharpTokenNode(Convert(spec.Location), Roles.LBracket), Roles.LBracket);
                    var location = LocationsBag.GetLocations(spec);
                    if (location != null)
                        aSpec.AddChild(new CSharpTokenNode(Convert(spec.Location), Roles.RBracket), Roles.RBracket);
                    result.AddChild(aSpec, ComposedType.ArraySpecifierRole);
                }
                spec = spec.Next;
            }

            return result;
        }

 
    
    
        public override object Visit(Invocation invocationExpression)
        {
            var result = new InvocationExpression();
            var location = LocationsBag.GetLocations(invocationExpression);
            if (invocationExpression.Exp != null)
                result.AddChild((Expression)invocationExpression.Exp.Accept(this), Roles.TargetExpression);
            if (location != null)
                result.AddChild(new CSharpTokenNode(Convert(location[0]), Roles.LPar), Roles.LPar);
            AddArguments(result, invocationExpression.Arguments);

            if (location != null && location.Count > 1)
                result.AddChild(new CSharpTokenNode(Convert(location[1]), Roles.RPar), Roles.RPar);
            return result;
        }

        public override object Visit(StackAlloc stackAllocExpression)
        {
            var result = new StackAllocExpression();

            var location = LocationsBag.GetLocations(stackAllocExpression);
            if (location != null)
                result.AddChild(new CSharpTokenNode(Convert(location[0]), StackAllocExpression.StackallocKeywordRole), StackAllocExpression.StackallocKeywordRole);
            if (stackAllocExpression.TypeExpression != null)
                result.AddChild(ConvertToType(stackAllocExpression.TypeExpression), Roles.Type);
            if (location != null && location.Count > 1)
                result.AddChild(new CSharpTokenNode(Convert(location[1]), Roles.LBracket), Roles.LBracket);
            if (stackAllocExpression.CountExpression != null)
                result.AddChild((Expression)stackAllocExpression.CountExpression.Accept(this), Roles.Expression);
            if (location != null && location.Count > 2)
                result.AddChild(new CSharpTokenNode(Convert(location[2]), Roles.RBracket), Roles.RBracket);
            return result;
        }
        #endregion



        #region XmlDoc

        public DocumentationReference ConvertXmlDoc(DocumentationBuilder doc)
        {
            var result = new DocumentationReference();
            if (doc.ParsedName != null)
            {
                if (doc.ParsedName.Name == "<this>")
                {
                    result.SymbolKind = SymbolKind.Indexer;
                }
                else
                {
                    result.MemberName = doc.ParsedName.Name;
                }
                if (doc.ParsedName.Left != null)
                {
                    result.DeclaringType = ConvertToType(doc.ParsedName.Left);
                }
                else if (doc.ParsedBuiltinType != null)
                {
                    result.DeclaringType = ConvertToType(doc.ParsedBuiltinType);
                }
                if (doc.ParsedName.TypeParameters != null)
                {
                    for (int i = 0; i < doc.ParsedName.TypeParameters.Count; i++)
                    {
                        result.TypeArguments.Add(ConvertToType(doc.ParsedName.TypeParameters[i]));
                    }
                }
            }
            else if (doc.ParsedBuiltinType != null)
            {
                result.SymbolKind = SymbolKind.TypeDefinition;
                result.DeclaringType = ConvertToType(doc.ParsedBuiltinType);
            }
            if (doc.ParsedParameters != null)
            {
                result.HasParameterList = true;
                result.Parameters.AddRange(doc.ParsedParameters.Select(ConvertXmlDocParameter));
            }
            if (doc.ParsedOperator != null)
            {
                result.SymbolKind = SymbolKind.Operator;
                result.OperatorType = (OperatorType)doc.ParsedOperator;
                if (result.OperatorType == OperatorType.Implicit || result.OperatorType == OperatorType.Explicit)
                {
                    var returnTypeParam = result.Parameters.LastOrNullObject();
                    returnTypeParam.Remove(); // detach from parameter list
                    var returnType = returnTypeParam.Type;
                    returnType.Remove();
                    result.ConversionOperatorReturnType = returnType;
                }
                if (result.Parameters.Count == 0)
                {
                    // reset HasParameterList if necessary
                    result.HasParameterList = false;
                }
            }
            return result;
        }

        ParameterDeclaration ConvertXmlDocParameter(DocumentationParameter p)
        {
            var result = new ParameterDeclaration();
            switch (p.Modifier)
            {
                case Parameter.Modifier.OUT:
                    result.ParameterModifier = ParameterModifier.Out;
                    break;
                case Parameter.Modifier.REF:
                    result.ParameterModifier = ParameterModifier.Ref;
                    break;
                case Parameter.Modifier.PARAMS:
                    result.ParameterModifier = ParameterModifier.Params;
                    break;
            }
            if (p.Type != null)
            {
                result.Type = ConvertToType(p.Type);
            }
            return result;
        }

        #endregion

    }
}
