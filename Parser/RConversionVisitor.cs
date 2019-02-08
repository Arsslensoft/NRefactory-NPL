using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using ICSharpCode.NRefactory.TypeSystem;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace ICSharpCode.NRefactory.Rava.Parser
{
    class ConversionVisitor : RSharpParserBaseVisitor<AstNode>
    {
        SyntaxTree unit = new SyntaxTree();
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

        static ConversionVisitor()
        {
            modifierTable["new"] = Modifiers.New;
            modifierTable["public"] = Modifiers.Public;
            modifierTable["protected"] = Modifiers.Protected;
            modifierTable["private"] = Modifiers.Private;
            modifierTable["friend"] = Modifiers.Internal;
            modifierTable["abstract"] = Modifiers.Abstract;
            modifierTable["virtual"] = Modifiers.Virtual;
            modifierTable["sealed"] = Modifiers.Sealed;
            modifierTable["static"] = Modifiers.Static;
            modifierTable["override"] = Modifiers.Override;
            modifierTable["readonly"] = Modifiers.Readonly;
            modifierTable["partial"] = Modifiers.Partial;
            modifierTable["extern"] = Modifiers.Extern;
            modifierTable["volatile"] = Modifiers.Volatile;
            modifierTable["unsafe"] = Modifiers.Unsafe;
            modifierTable["async"] = Modifiers.Async;
            modifierTable["sync"] = Modifiers.Sync;
        }
        public static TextLocation Convert(IToken loc)
        {
            return new TextLocation(loc.Line, loc.Column+1);
        }
        public static TextLocation Convert(ITerminalNode loc)
        {
            return new TextLocation(loc.Symbol.Line, loc.Symbol.Column+1);
        }
        readonly Stack<NamespaceDeclaration> namespaceStack = new Stack<NamespaceDeclaration>();
        // TODO:Initialize mods table
        static readonly Dictionary<string, Modifiers> modifierTable = new Dictionary<string, Modifiers>();
        static void AddModifiers(EntityDeclaration parent, RSharpParser.Member_modifiersContext mods)
        {
            if (mods == null)
                return;

            foreach (var modifier in mods.member_modifier())
            {
                Modifiers mod;
                if (!modifierTable.TryGetValue(modifier.GetText(), out mod))
                    Console.WriteLine("modifier " + modifier.GetText() + " can't be converted,");
                parent.AddChild(new CSharpModifierToken(Convert(modifier.start), mod), EntityDeclaration.ModifierRole);
            }
        }
        static void AddModifiers(EntityDeclaration parent, RSharpParser.Type_modifiersContext mods)
        {
            if (mods == null)
                return;

            foreach (var modifier in mods.type_modifier())
            {
                Modifiers mod;
                if (!modifierTable.TryGetValue(modifier.GetText(), out mod))
                    Console.WriteLine("modifier " + modifier.GetText() + " can't be converted,");
                parent.AddChild(new CSharpModifierToken(Convert(modifier.start), mod), EntityDeclaration.ModifierRole);
            }
        }
        static void AddModifiers(EntityDeclaration parent, RSharpParser.Method_modifiersContext mods)
        {
            if (mods == null)
                return;

            foreach (var modifier in mods.method_modifier())
            {
                Modifiers mod;
                if (!modifierTable.TryGetValue(modifier.GetText(), out mod))
                    Console.WriteLine("modifier " + modifier.GetText() + " can't be converted,");
                parent.AddChild(new CSharpModifierToken(Convert(modifier.start), mod), EntityDeclaration.ModifierRole);
            }
        }
        static void AddModifiers(EntityDeclaration parent, RSharpParser.Accessor_modifierContext modifier)
        {
            if (modifier == null)
                return;
                Modifiers mod;
                if (!modifierTable.TryGetValue(modifier.GetText(), out mod))
                    Console.WriteLine("modifier " + modifier.GetText() + " can't be converted,");
                parent.AddChild(new CSharpModifierToken(Convert(modifier.start), mod), EntityDeclaration.ModifierRole);
            
        }


        // TODO:Add Explicit interface
        void AddExplicitInterface(AstNode parent, RSharpParser.Member_nameContext memberName)
        {
            //if (memberName == null || memberName.ExplicitInterface == null)
            //    return;

            //parent.AddChild(ConvertToType(memberName.ExplicitInterface), EntityDeclaration.PrivateImplementationTypeRole);
            //var privateImplTypeLoc = LocationsBag.GetLocations(memberName.ExplicitInterface);
            //if (privateImplTypeLoc != null)
            //    parent.AddChild(new CSharpTokenNode(Convert(privateImplTypeLoc[0]), Roles.Dot), Roles.Dot);
        }

        void AddParameters(AstNode parent, RSharpParser.Explicit_anonymous_function_parameter_listContext exp)
        {
            var parameters = exp.explicit_anonymous_function_parameter();
            if (parameters == null)
                return;
           
            for (int i = 0; i < parameters.Length; i++)
            {
                var p = parameters[i];
                var parameterDeclarationExpression = new ParameterDeclaration();

                var rf = p.REF();
                var ot = p.OUT();
                if (rf == null)
                {
                    parameterDeclarationExpression.ParameterModifier = ParameterModifier.Out;
                    parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(rf), ParameterDeclaration.OutModifierRole), ParameterDeclaration.OutModifierRole);
                }
                else
                {
                    parameterDeclarationExpression.ParameterModifier = ParameterModifier.Ref;
                    parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(ot), ParameterDeclaration.RefModifierRole), ParameterDeclaration.RefModifierRole);
                }
               parameterDeclarationExpression.AddChild(ConvertToType(p.type()), Roles.Type);
               var id = p.identifier();
               parameterDeclarationExpression.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
               parent.AddChild(parameterDeclarationExpression, Roles.Parameter);
               var comma = exp.COMMA(i);
                if(comma != null)
                    parent.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
               
            }
        }
        void AddParameters(AstNode parent, RSharpParser.Formal_parameter_listContext exp)
        {
           
            if (exp == null)
                return;
            var fixedparameters = exp.fixed_parameters();
            if (fixedparameters != null)
            {
                var parameters = fixedparameters.fixed_parameter();
                for (int i = 0; i < parameters.Length; i++)
                {
                    var p = parameters[i];
                    var parameterDeclarationExpression = new ParameterDeclaration();
                    AddAttributeSection(parameterDeclarationExpression, p.attributes());
                    var pm = p.parameter_modifier();
                    if (pm != null)
                    {
                        var rf = pm.REF();
                        var ot = pm.OUT();
                        var slf = pm.SELF();
                        if (rf != null)
                        {
                            parameterDeclarationExpression.ParameterModifier = ParameterModifier.Ref;
                            parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(rf), ParameterDeclaration.RefModifierRole), ParameterDeclaration.RefModifierRole);
                        }
                        else if (slf != null)
                        {
                            parameterDeclarationExpression.ParameterModifier = ParameterModifier.This;
                            parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(slf), ParameterDeclaration.ThisModifierRole), ParameterDeclaration.ThisModifierRole);
                        }
                        else
                        {
                            parameterDeclarationExpression.ParameterModifier = ParameterModifier.Out;
                            parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(ot), ParameterDeclaration.OutModifierRole), ParameterDeclaration.OutModifierRole);
                        }
                    }
                    var arg_decl = p.arg_declaration();
                    parameterDeclarationExpression.AddChild(ConvertToType(arg_decl.type()), Roles.Type);
                    var id = arg_decl.identifier();
                    parameterDeclarationExpression.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                    parent.AddChild(parameterDeclarationExpression, Roles.Parameter);
                    var comma = fixedparameters.COMMA(i);
                    if (comma != null)
                        parent.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);

                }
            }

            var param = exp.parameter_array();
            if (param != null)
            {
                var parameterDeclarationExpression = new ParameterDeclaration();
                AddAttributeSection(parameterDeclarationExpression, param.attributes());
                parameterDeclarationExpression.ParameterModifier = ParameterModifier.Params;
                parameterDeclarationExpression.AddChild(new CSharpTokenNode(Convert(param.PARAMS()), ParameterDeclaration.ParamsModifierRole), ParameterDeclaration.ParamsModifierRole);
                parameterDeclarationExpression.AddChild(ConvertToType(param.array_type()), Roles.Type);
                var id = param.identifier();
                parameterDeclarationExpression.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                parent.AddChild(parameterDeclarationExpression, Roles.Parameter);

            }
        }
        void AddParameter(AstNode parent, RSharpParser.Arg_declarationContext exp)
        {

            if (exp == null)
                return;
         
             
                var parameterDeclarationExpression = new ParameterDeclaration();
                parameterDeclarationExpression.ParameterModifier = ParameterModifier.None;
                parameterDeclarationExpression.AddChild(ConvertToType(exp.type()), Roles.Type);
                var id = exp.identifier();
                parameterDeclarationExpression.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                parent.AddChild(parameterDeclarationExpression, Roles.Parameter);

            
        }
        void AddParameters(AstNode parent, RSharpParser.Implicit_anonymous_function_parameter_listContext exp)
        {
            var parameters = exp.identifier();
            if (parameters == null)
                return;

            for (int i = 0; i < parameters.Length; i++)
            {
                var p = parameters[i];
                var parameterDeclarationExpression = new ParameterDeclaration();
                parameterDeclarationExpression.AddChild(Identifier.Create(p.GetText(), Convert(p.start)), Roles.Identifier);
                parent.AddChild(parameterDeclarationExpression, Roles.Parameter);
                var comma = exp.COMMA(i);
                if (comma != null)
                    parent.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);

            }
        }
        void AddParameters(AstNode parent, RSharpParser.IdentifierContext exp)
        {

                var parameterDeclarationExpression = new ParameterDeclaration();
                parameterDeclarationExpression.AddChild(Identifier.Create(exp.GetText(), Convert(exp.start)), Roles.Identifier);
                parent.AddChild(parameterDeclarationExpression, Roles.Parameter);
             
         
        }
        Expression ConvertArgument(RSharpParser.Indexer_argumentContext arg)
        {
            var na = arg.identifier();
            var expr = arg.expression();
            if (na != null)
            {
               
                var colon = arg.COLON();
                var newArg = new NamedArgumentExpression();
                newArg.AddChild(Identifier.Create(na.GetText(), Convert(na.start)), Roles.Identifier);
                newArg.AddChild(new CSharpTokenNode(Convert(colon), Roles.Colon), Roles.Colon);
                newArg.AddChild((Expression)expr.Accept(this), Roles.Expression);
                return newArg;
            }


            return (Expression)expr.Accept(this);
        }
        Expression ConvertArgument(RSharpParser.ArgumentContext arg)
        {
            var na = arg.identifier();
            var expr = arg.expression();
            if (na != null)
            {

                var colon = arg.COLON();
                var newArg = new NamedArgumentExpression();
                newArg.AddChild(Identifier.Create(na.GetText(), Convert(na.start)), Roles.Identifier);
                newArg.AddChild(new CSharpTokenNode(Convert(colon), Roles.Colon), Roles.Colon);

                var rf = arg.REF();
                var ot = arg.OUT();
                if (rf != null || ot != null)
                {
                    var direction = new DirectionExpression();
                    direction.FieldDirection = rf == null ? FieldDirection.Out : FieldDirection.Ref;
                    var r = rf == null ? DirectionExpression.OutKeywordRole : DirectionExpression.RefKeywordRole;
                    direction.AddChild(new CSharpTokenNode(Convert(arg.refout), r), r);
                    direction.AddChild((Expression)expr.Accept(this), Roles.Expression);
                    newArg.AddChild(direction, Roles.Expression);
                }
                else newArg.AddChild((Expression)expr.Accept(this), Roles.Expression);
              
                return newArg;
            }


            var srf = arg.REF();
            var sot = arg.OUT();
            if (srf != null || sot != null)
            {
                var direction = new DirectionExpression();
                direction.FieldDirection = srf == null ? FieldDirection.Out : FieldDirection.Ref;
                var r = srf == null ? DirectionExpression.OutKeywordRole : DirectionExpression.RefKeywordRole;
                direction.AddChild(new CSharpTokenNode(Convert(arg.refout), r), r);
                direction.AddChild((Expression)expr.Accept(this), Roles.Expression);
                return direction;
            }

            return (Expression)expr.Accept(this);
        }
        Expression ConvertArgument(RSharpParser.Attribute_argumentContext arg)
        {
            var na = arg.identifier();
            var expr = arg.expression();
            if (na != null)
            {

                var colon = arg.COLON();
                var newArg = new NamedArgumentExpression();
                newArg.AddChild(Identifier.Create(na.GetText(), Convert(na.start)), Roles.Identifier);
                newArg.AddChild(new CSharpTokenNode(Convert(colon), Roles.Colon), Roles.Colon);
                newArg.AddChild((Expression)expr.Accept(this), Roles.Expression);
                return newArg;
            }


            return (Expression)expr.Accept(this);
        }
        void AddArguments(AstNode parent, RSharpParser.Bracket_expressionContext bec)
        {
            var args = bec.indexer_argument();
            if (args == null)
                return;

            for (int i = 0; i < args.Length; i++)
            {
                parent.AddChild(ConvertArgument(args[i]), Roles.Argument);
                var coma = bec.COMMA(i);
                if(coma != null)
                    parent.AddChild(new CSharpTokenNode(Convert(coma), Roles.Comma), Roles.Comma);
                
            }
            var scoma = bec.COMMA(args.Length - 1 );
            if (scoma != null)
                parent.AddChild(new CSharpTokenNode(Convert(scoma), Roles.Comma), Roles.Comma);
                
        }
        void AddArguments(AstNode parent, RSharpParser.Argument_listContext bec)
        {
            if (bec == null)
                return;
            var args = bec.argument();
            if (args == null)
                return;

            for (int i = 0; i < args.Length; i++)
            {
                parent.AddChild(ConvertArgument(args[i]), Roles.Argument);
                var coma = bec.COMMA(i);
                if (coma != null)
                    parent.AddChild(new CSharpTokenNode(Convert(coma), Roles.Comma), Roles.Comma);

            }
            var scoma = bec.COMMA(args.Length - 1);
            if (scoma != null)
                parent.AddChild(new CSharpTokenNode(Convert(scoma), Roles.Comma), Roles.Comma);

        }
        Expression AddBracketExpressions(Expression first, RSharpParser.Element_access_expressionContext eae)
        {
            Expression last = first;
            foreach (RSharpParser.Bracket_expressionContext bec in eae.bracket_expression())
            {
                var result = new IndexerExpression();
                result.AddChild(last, Roles.TargetExpression);
                var interr = bec.INTERR();
                if(interr != null)
                    result.AddChild(new CSharpTokenNode(Convert(interr), Roles.Interr), Roles.Interr);

                result.AddChild(new CSharpTokenNode(Convert(bec.OPEN_BRACKET()), Roles.LBracket), Roles.LBracket);

                AddArguments(result, bec);
                result.AddChild(new CSharpTokenNode(Convert(bec.CLOSE_BRACKET()), Roles.RBracket), Roles.RBracket);
                last = result;
            }
            return last;
        }
        public ExpressionStatement CreateExpressionStatement(RSharpParser.ExpressionContext ex)
        {
            var result = new ExpressionStatement();
           
            var expr = ex.Accept(this) as Expression;
            if (expr != null)
                result.AddChild(expr, Roles.Expression);

            result.AddChild(new CSharpTokenNode(Convert(ex.Stop), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public ReturnStatement CreateReturnStatement(RSharpParser.ExpressionContext ex)
        {
            var result = new ReturnStatement();

            result.AddChild(new CSharpTokenNode(Convert(ex.Start), ReturnStatement.ReturnKeywordRole), ReturnStatement.ReturnKeywordRole);
   
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);

            result.AddChild(new CSharpTokenNode(Convert(ex.stop), Roles.Semicolon), Roles.Semicolon);

            return result;
        }
        public BlockStatement CreateArrowStatement(RSharpParser.ExpressionContext ex)
        {
            var result = new BlockStatement();
            result.AddChild(new CSharpTokenNode(Convert(ex.start), Roles.LBrace), Roles.LBrace);
            result.AddChild(CreateReturnStatement(ex), BlockStatement.StatementRole);
            result.AddChild(new CSharpTokenNode(Convert(ex.stop), Roles.RBrace), Roles.RBrace);
            return result;
        }

        void AddBlockChildren(BlockStatement result, RSharpParser.BlockContext blockStatement)
        {
         var stms = blockStatement.statement_list();
         if (stms == null)
             return;
            foreach (var stmt in stms.statement())
            {
                if (stmt == null)
                    continue;

                if (stmt is RSharpParser.EmbeddedStatementContext)
                {
                    var eb = (stmt as RSharpParser.EmbeddedStatementContext).embedded_statement();
                    var bl = eb.block();
                    if(bl != null)
                        AddBlockChildren(result, bl);
                    else result.AddChild((Statement)stmt.Accept(this), BlockStatement.StatementRole);
                }
                else
                    result.AddChild((Statement)stmt.Accept(this), BlockStatement.StatementRole);
                
            }
        }
        void AddTypeArguments(RSharpParser.Type_argument_listContext texpr, AstNode result)
        {
            if (texpr == null)
                return;
           var args = texpr.type();
            if (args == null)
                return;

           result.AddChild(new CSharpTokenNode(Convert(texpr.LT().Symbol), Roles.LChevron), Roles.LChevron);
            int i = 0;
            foreach (var arg in args)
            {
                result.AddChild(ConvertToType(arg), Roles.TypeArgument);
                var comma = texpr.COMMA(i);
                if (comma != null)
                {
                    result.AddChild(new CSharpTokenNode(Convert(comma.Symbol), Roles.Comma), Roles.Comma);
                    i++;
                }
            }
           
            result.AddChild(new CSharpTokenNode(Convert(texpr.GT().Symbol), Roles.RChevron), Roles.RChevron);
        }
        void AddTypeParameters(AstNode parent, RSharpParser.Type_parameter_listContext par)
        {
            if (par == null)
                return;
            var para = par.type_parameter();
            parent.AddChild(new CSharpTokenNode(Convert(par.LT()), Roles.LChevron), Roles.LChevron);
            for (int i = 0; i < para.Length; i++)
            {

                var arg = para[i];
                if (arg == null)
                    continue;
                var tp = new TypeParameterDeclaration();
               tp.Variance = VarianceModifier.Invariant;
               var id = arg.identifier();
               tp.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                parent.AddChild(tp, Roles.TypeParameter);
            }
         parent.AddChild(new CSharpTokenNode(Convert(par.GT()), Roles.RChevron), Roles.RChevron);
        }
        void AddConstraints(AstNode parent, RSharpParser.Type_parameter_constraints_clausesContext constr)
        {
            if (constr == null)
                return;
            var d = constr.type_parameter_constraints_clause();
            for (int i = 0; i < d.Length; i++)
            {
                var typeParameter = d[i];
                if (typeParameter == null)
                    continue;

                var c = typeParameter.type_parameter_constraints();
                if (c == null)
                    continue;


                var constraint = new Constraint();
                constraint.AddChild(new CSharpTokenNode(Convert(typeParameter.WHERE()), Roles.WhereKeyword), Roles.WhereKeyword);
                var tp = typeParameter.identifier();
                constraint.AddChild(new SimpleType(Identifier.Create(tp.GetText(), Convert(tp.start))), Roles.ConstraintTypeParameter);
                constraint.AddChild(new CSharpTokenNode(Convert(typeParameter.COLON()), Roles.Colon), Roles.Colon);
        
                int curComma = 0;
                var pco =  c.primary_constraint();
                var sco = c.secondary_constraints();
                var cl = pco.CLASS();
                var clt = pco.class_type();
                 
                if(cl != null)
                    constraint.AddChild(new PrimitiveType("class", Convert(cl)), Roles.BaseType);

                if (clt != null)
                    constraint.AddChild(ConvertToType(clt), Roles.BaseType);

                if (sco != null)
                {
                    foreach(var p in sco.package_or_type_name())
                        constraint.AddChild(ConvertToType(p), Roles.BaseType);
                }
                // We need to sort the constraints by position; as they might be in a different order than the type parameters
                AstNode prevSibling = parent.LastChild;
                while (prevSibling.StartLocation > constraint.StartLocation && prevSibling.PrevSibling != null)
                    prevSibling = prevSibling.PrevSibling;
                parent.InsertChildAfter(prevSibling, constraint, Roles.Constraint);
            }
        }
        AstType ConvertToType(RSharpParser.IdentifierContext ident)
        {
            return new SimpleType { IdentifierToken = Identifier.Create(ident.GetText(), Convert(ident.start)) };
           
        }
        AstType ConvertToType(RSharpParser.Class_typeContext ident)
        {
            var pkg = ident.package_or_type_name();
            if (pkg != null)
                return ConvertToType(pkg);
            else return new PrimitiveType(ident.GetText(), Convert(ident.start));

        }
        AstType ConvertToType(RSharpParser.Predefined_typeContext ident)
        {
            return new SimpleType { IdentifierToken = Identifier.Create(ident.GetText(), Convert(ident.start)) };

        }
        static AstType ConvertToType(RSharpParser.Type_parameterContext spec)
        {
            AstType result;
            var ident = spec.identifier();
            result = new SimpleType { IdentifierToken = Identifier.Create(ident.GetText(), Convert(ident.start)) };
            return result;
        }
        AstType ConvertToType(RSharpParser.TypeContext typeName)
        {
                var baseType = ConvertToType(typeName.base_type());
                var result = new ComposedType { BaseType = baseType };
                var typespec = typeName.type_specifier();
                if (typespec == null)
                    return baseType;
                else
                {
                    foreach (var ts in typespec)
                    {
                        var st = ts.STAR();
                        var nullable = ts.INTERR();
                        var arr = ts.rank_specifier();
                        if(st != null)
                            result.AddChild(new CSharpTokenNode(Convert(st), ComposedType.PointerRole), ComposedType.PointerRole);
                        else if(nullable != nullable)
                            result.AddChild(new CSharpTokenNode(Convert(nullable), ComposedType.NullableRole), ComposedType.NullableRole);
                        else
                        {
                            var spec= arr.Accept(this);
                            result.ArraySpecifiers.Add((ArraySpecifier)spec);
                        }
                    }
                 
                    return result;
                }
       
            return new SimpleType(typeName.GetText(), Convert(typeName.Start));
        } // TODO:Fix
        AstType ConvertToType(RSharpParser.Integral_typeContext typeName)
        {
            return new PrimitiveType(typeName.GetText(), Convert(typeName.Start));
        } // TODO:Fix

        AstType ConvertToType(RSharpParser.Return_typeContext typeName)
        {
            return new SimpleType(typeName.GetText(), Convert(typeName.Start));
        } // TODO:Fix
        AstType ConvertToType(RSharpParser.Array_typeContext typeName)
        {
            return new SimpleType(typeName.GetText(), Convert(typeName.Start));
        } // TODO:Fix
        AstType ConvertToType(RSharpParser.Base_typeContext typeName)
        {

            return new PrimitiveType(typeName.GetText(), Convert(typeName.start));
        }
        AstType ConvertToType(RSharpParser.Qualified_alias_memberContext typeName)
        {
            var ident = typeName.identifier(1);
            var memberType = new MemberType();
            memberType.Target = ConvertToType(typeName.identifier(0));
            memberType.IsDoubleColon = true;
            memberType.AddChild(new CSharpTokenNode(Convert(typeName.DOUBLE_COLON().Symbol), Roles.DoubleColon), Roles.DoubleColon);
            memberType.MemberNameToken = Identifier.Create(ident.GetText(), Convert(ident.start));
            AddTypeArguments(typeName.type_argument_list(), memberType);
            return memberType;
        }
        AstType ConvertNamespaceName(RSharpParser.Qualified_identifierContext memberName)
        {
      
            return ConvertToType(memberName);
        }
        AstType ConvertToType(RSharpParser.Qualified_identifierContext qi)
        {
           
            var idents = qi.identifier();
        if(idents.Length > 1)
                {
                int i = 0;
                AstType target = null;
              AstType result = null;
                for (i = 0; i < idents.Length; i++)
                {
                    if (i == 0)
                    {
                      target =  new SimpleType { IdentifierToken = Identifier.Create(idents[i].GetText(), Convert(idents[i].start)) };
                      continue;
                    }
                    result = new MemberType();
                    result.AddChild(target, MemberType.TargetRole);
                    var dot = qi.DOT(i);
                    if(dot != null)
                        result.AddChild(new CSharpTokenNode(Convert(dot.Symbol), Roles.Dot), Roles.Dot);
                    result.AddChild(Identifier.Create(idents[i].GetText(), Convert(idents[i].start)), Roles.Identifier);
                    target = result;
                }

                return target;
            }
            else
                    return new SimpleType { IdentifierToken = Identifier.Create(idents[0].GetText(), Convert(idents[0].start)) };
      
        }
        AstType ConvertToType(RSharpParser.Package_or_type_nameContext typeName)
        {
            if (typeName == null) // may happen in typeof(Generic<,,,,>)
                return new SimpleType();

            AstType target = null;
            var sn = typeName.simple_name();           
            var qam = typeName.qualified_alias_member();
            if (sn != null)
            {
                target = ConvertToType(sn.identifier());
                AddTypeArguments(sn.type_argument_list(), target);
            }
            else
                target = ConvertToType(qam);

            var idents = typeName.simple_name_access();
            MemberType result = new MemberType();
            for (int i = 0; i < idents.Length; i++)
            {
                result = new MemberType();
                result.AddChild(target, MemberType.TargetRole);
                result.AddChild(new CSharpTokenNode(Convert(typeName.DOT(i).Symbol), Roles.Dot), Roles.Dot);
                result.MemberNameToken = Identifier.Create(idents[i].identifier().GetText(), Convert(idents[i].start));
                AddTypeArguments(idents[i].type_argument_list(), result);
                target = result;
            }

            return target;
        }

        IEnumerable<Attribute> GetAttributes(RSharpParser.Attribute_listContext optAttributes)
        {
            if (optAttributes == null)
                yield break;
            foreach (var attr in optAttributes.attribute())
            {
                var result = new Attribute();
                result.Type = ConvertToType(attr.package_or_type_name());
                var args = attr.attribute_argument();
                result.HasArgumentList = args != null;
                int pos = 0;
                if (args != null)
                    result.AddChild(new CSharpTokenNode(Convert(attr.OPEN_PARENS().Symbol), Roles.LPar), Roles.LPar);
                foreach (var arg in args)
                {
                    if (arg == null) continue;
                    var ident = arg.identifier();
                    if (ident != null) // named arg
                    {
                     
                        var newArg = new NamedArgumentExpression();
                        newArg.AddChild(Identifier.Create(ident.GetText(), Convert(ident.Start)), Roles.Identifier);
                        newArg.AddChild(new CSharpTokenNode(Convert(arg.COLON().Symbol), Roles.Colon), Roles.Assign);
                         newArg.AddChild((Expression)arg.expression().Accept(this), Roles.Expression);
                         result.AddChild(newArg, Roles.Argument);
                        result.AddChild(new CSharpTokenNode(Convert(arg.Stop), Roles.Comma), Roles.Comma);
                    }
                    else
                    {
                        var newArg = new NamedArgumentExpression();
                        newArg.AddChild((Expression)arg.expression().Accept(this), Roles.Expression);
                        result.AddChild(newArg, Roles.Argument);
                        result.AddChild(new CSharpTokenNode(Convert(arg.Stop), Roles.Comma), Roles.Comma);
                    }
                }


                result.AddChild(new CSharpTokenNode(Convert(attr.CLOSE_PARENS().Symbol), Roles.RPar), Roles.RPar);

                yield return result;
            }
        }
        AttributeSection ConvertAttributeSection(RSharpParser.Attribute_sectionContext optAttributes)
        {
            if (optAttributes == null)
                return null;
            var result = new AttributeSection();
            var loc = optAttributes.Start;
            int pos = 0;
            if (loc != null)
                result.AddChild(new CSharpTokenNode(Convert(optAttributes.OPEN_BRACKET().Symbol), Roles.LBracket), Roles.LBracket);
            var target = optAttributes.attribute_target();

            if (target !=null)
            {
                    result.AddChild(Identifier.Create(target.GetText(), Convert(target.start)), Roles.Identifier);
                    result.AddChild(new CSharpTokenNode(Convert(optAttributes.COLON().Symbol), Roles.Colon), Roles.Colon);
            }

            int attributeCount = 0;
            foreach (var attr in GetAttributes(optAttributes.attribute_list()))
            {
                result.AddChild(attr, Roles.Attribute);
                result.AddChild(new CSharpTokenNode(attr.EndLocation, Roles.Comma), Roles.Comma);

                attributeCount++;
            }
            if (attributeCount == 0)
                return null;

                result.AddChild(new CSharpTokenNode(Convert(optAttributes.CLOSE_BRACKET().Symbol), Roles.RBracket), Roles.RBracket);
            return result;
        }
        AttributeSection ConvertAttributeSection(RSharpParser.Global_attribute_sectionContext optAttributes)
        {
            if (optAttributes == null)
                return null;
            var result = new AttributeSection();
            var loc = optAttributes.Start;
            int pos = 0;
            if (loc != null)
                result.AddChild(new CSharpTokenNode(Convert(optAttributes.OPEN_BRACKET().Symbol), Roles.LBracket), Roles.LBracket);
            var target = optAttributes.global_attribute_target();

            if (target != null)
            {
                result.AddChild(Identifier.Create(target.GetText(), Convert(target.start)), Roles.Identifier);
                result.AddChild(new CSharpTokenNode(Convert(optAttributes.COLON().Symbol), Roles.Colon), Roles.Colon);
            }

            int attributeCount = 0;
            foreach (var attr in GetAttributes(optAttributes.attribute_list()))
            {
                result.AddChild(attr, Roles.Attribute);
                result.AddChild(new CSharpTokenNode(attr.EndLocation, Roles.Comma), Roles.Comma);

                attributeCount++;
            }
            if (attributeCount == 0)
                return null;

            result.AddChild(new CSharpTokenNode(Convert(optAttributes.CLOSE_BRACKET().Symbol), Roles.RBracket), Roles.RBracket);
            return result;
        }

        public void AddAttributeSection(AstNode parent, RSharpParser.AttributesContext attrs, Role<AttributeSection> role)
        {
            if (attrs == null)
                return;
            foreach (var attr in attrs.attribute_section())
            {
                var section = ConvertAttributeSection(attr);
                if (section == null)
                    continue;
                parent.AddChild(section, role);
            }
        }
        public void AddAttributeSection(AstNode parent, RSharpParser.AttributesContext attrs)
        {
            AddAttributeSection(parent, attrs, EntityDeclaration.AttributeRole);
        }
        public void AddAttributeSection(AstNode parent, RSharpParser.Global_attribute_sectionContext[] attrs, Role<AttributeSection> role)
        {
            if (attrs == null)
                return;
            foreach (var attr in attrs)
            {
                var section = ConvertAttributeSection(attr);
                if (section == null)
                    continue;
                parent.AddChild(section, role);
            }
        }
        public void AddAttributeSection(AstNode parent, RSharpParser.Global_attribute_sectionContext[] attrs)
        {
            AddAttributeSection(parent, attrs, EntityDeclaration.AttributeRole);
        }

        void AddToNamespace(AstNode child)
        {
            if (namespaceStack.Count > 0)
            {
                namespaceStack.Peek().AddChild(child, NamespaceDeclaration.MemberRole);
            }
            else
            {
                unit.AddChild(child, SyntaxTree.MemberRole);
            }
        }
        void AddType(EntityDeclaration child)
        {
            if (typeStack.Count > 0)
            {
                typeStack.Peek().AddChild(child, Roles.TypeMemberRole);
            }
            else
            {
                AddToNamespace(child);
            }
        }

        public override AstNode VisitCompilation_unit(RSharpParser.Compilation_unitContext context)
        {
      
            var imports = context.import_directives();
            if (imports != null)
            {
                foreach (var us in imports.import_directive())
                    us.Accept(this);
            }

            var pmd = context.package_member_declarations();
        
          if(pmd != null)
            foreach (var container in pmd.package_member_declaration())             
                    container.Accept(this);
         
     // global attributes
          AddAttributeSection(unit,context.global_attribute_section(), EntityDeclaration.UnattachedAttributeRole);

          return unit;
        }
        public override AstNode VisitImportPackageDirective(RSharpParser.ImportPackageDirectiveContext context)
        {
            var ud = new IncludeDeclaration();
            ud.AddChild(new CSharpTokenNode(Convert(context.IMPORT().Symbol), IncludeDeclaration.UsingKeywordRole), IncludeDeclaration.UsingKeywordRole);
            ud.AddChild(ConvertToType(context.package_or_type_name()), IncludeDeclaration.ImportRole);
            ud.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);
            AddToNamespace(ud);
            return ud;
        }
        public override AstNode VisitImportAliasDirective(RSharpParser.ImportAliasDirectiveContext context)
        {
            var ud = new IncludeAliasDeclaration();
            var id = context.identifier();
            ud.AddChild(new CSharpTokenNode(Convert(context.IMPORT().Symbol), IncludeAliasDeclaration.UsingKeywordRole), IncludeAliasDeclaration.UsingKeywordRole);
            ud.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), IncludeAliasDeclaration.AliasRole);
                ud.AddChild(new CSharpTokenNode(Convert(context.ASSIGNMENT().Symbol), Roles.Assign), Roles.Assign);
                ud.AddChild(ConvertToType(context.package_or_type_name()), IncludeAliasDeclaration.ImportRole);
                ud.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);
            AddToNamespace(ud);
            return ud;
        }


      
        #region jump statements
        public override AstNode VisitYieldBreakStatement(RSharpParser.YieldBreakStatementContext context)
        {
            var result = new YieldBreakStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.YIELD().Symbol), YieldBreakStatement.YieldKeywordRole), YieldBreakStatement.YieldKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.BREAK().Symbol), YieldReturnStatement.YieldKeywordRole), YieldReturnStatement.YieldKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);
    
            return result;
        }
        public override AstNode VisitYieldReturnStatement(RSharpParser.YieldReturnStatementContext context)
        {
            var result = new YieldReturnStatement();

            result.AddChild(new CSharpTokenNode(Convert(context.YIELD().Symbol), YieldReturnStatement.YieldKeywordRole), YieldReturnStatement.YieldKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.RETURN().Symbol), YieldReturnStatement.YieldKeywordRole), YieldReturnStatement.YieldKeywordRole);
         var ex = context.expression();
         if (ex != null)
             result.AddChild((Expression)ex.Accept(this), Roles.Expression);

            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);

            return result;
        }
        public override AstNode VisitBreakStatement(RSharpParser.BreakStatementContext context)
        {
            var result = new BreakStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.BREAK().Symbol), BreakStatement.BreakKeywordRole), BreakStatement.BreakKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public override AstNode VisitContinueStatement(RSharpParser.ContinueStatementContext context)
        {
            var result = new ContinueStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.CONTINUE().Symbol), ContinueStatement.ContinueKeywordRole), ContinueStatement.ContinueKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON().Symbol), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public override AstNode VisitReturnStatement(RSharpParser.ReturnStatementContext context)
        {
            var result = new ReturnStatement();

            result.AddChild(new CSharpTokenNode(Convert(context.RETURN().Symbol), ReturnStatement.ReturnKeywordRole), ReturnStatement.ReturnKeywordRole);
            var ex = context.expression();
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);

                result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

            return result;
        }
        public override AstNode VisitThrowStatement(RSharpParser.ThrowStatementContext context)
        {
            var result = new ThrowStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.THROW()), ThrowStatement.ThrowKeywordRole), ThrowStatement.ThrowKeywordRole);
            var ex = context.expression();
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

            return result;
        }
        public override AstNode VisitGotoCaseStatement(RSharpParser.GotoCaseStatementContext context)
        {
            var result = new GotoCaseStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.GOTO()), GotoCaseStatement.GotoKeywordRole), GotoCaseStatement.GotoKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.CASE()), GotoCaseStatement.CaseKeywordRole), GotoCaseStatement.CaseKeywordRole);
                var ex = context.expression();
                if (ex != null)
                    result.AddChild((Expression)ex.Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public override AstNode VisitGotoDefaultStatement(RSharpParser.GotoDefaultStatementContext context)
        {
            var result = new GotoDefaultStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.GOTO()), GotoCaseStatement.GotoKeywordRole), GotoCaseStatement.GotoKeywordRole); 
        
                result.AddChild(new CSharpTokenNode(Convert(context.DEFAULT()), GotoDefaultStatement.DefaultKeywordRole), GotoDefaultStatement.DefaultKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
          

            return result;
        }
        public override AstNode VisitGotoStatement(RSharpParser.GotoStatementContext context)
        {
            var result = new GotoStatement();
            var id = context.identifier();
            result.AddChild(new CSharpTokenNode(Convert(context.GOTO()), GotoCaseStatement.GotoKeywordRole), GotoCaseStatement.GotoKeywordRole); 
        
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        #endregion

        public override AstNode VisitLabeledStatement(RSharpParser.LabeledStatementContext context)
        {
            var result = new LabelStatement();
            var id = context.identifier();
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
    
                result.AddChild(new CSharpTokenNode(Convert(context.COLON()), Roles.Colon), Roles.Colon);
            return result;
        }
        public override AstNode VisitBlock(RSharpParser.BlockContext context)
        {
            var result = new BlockStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            AddBlockChildren(result, context);
            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
            return result;
        }
        public override AstNode VisitStatement_list(RSharpParser.Statement_listContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitEmbeddedStatement(RSharpParser.EmbeddedStatementContext context)
        {
            return context.embedded_statement().Accept(this);
        }
        public override AstNode VisitEmbedded_statement(RSharpParser.Embedded_statementContext context)
        {
            var bl = context.block();
            if (bl != null)
                return bl.Accept(this);
            else return context.simple_embedded_statement().Accept(this);
        }
        public override AstNode VisitEmptyStatement(RSharpParser.EmptyStatementContext context)
        {
            var result = new EmptyStatement();
            result.Location = Convert(context.SEMICOLON());
            return result;
        }
        public override AstNode VisitExpressionStatement(RSharpParser.ExpressionStatementContext context)
        {
            var result = new ExpressionStatement();
            var ex = context.expression();
            if (ex == null)
                return result;
            var expr = ex.Accept(this) as Expression;
            if (expr != null)
                result.AddChild(expr, Roles.Expression);
      
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public override AstNode VisitIfStatement(RSharpParser.IfStatementContext context)
        {
            var result = new IfElseStatement();

            result.AddChild(new CSharpTokenNode(Convert(context.IF()), IfElseStatement.IfKeywordRole), IfElseStatement.IfKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            var ex = context.expression();
            if (ex == null)
                    result.AddChild((Expression)ex.Accept(this), Roles.Condition);

            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            var tr = context.if_body(0);
            if (tr != null)
                result.AddChild((Statement)tr.Accept(this), IfElseStatement.TrueRole);

            var el = context.ELSE();
            if (el != null)
            {
                var fl = context.if_body(1);
                result.AddChild(new CSharpTokenNode(Convert(el), IfElseStatement.ElseKeywordRole), IfElseStatement.ElseKeywordRole);
                result.AddChild((Statement)fl.Accept(this), IfElseStatement.FalseRole);
            }

            return result;
        }
        public override AstNode VisitIf_body(RSharpParser.If_bodyContext context)
        {
            var bl = context.block();
            if (bl != null)
                return bl.Accept(this);
            else return context.simple_embedded_statement().Accept(this);
        }
        public override AstNode VisitSwitchStatement(RSharpParser.SwitchStatementContext context)
        {
            var result = new SwitchStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.SWITCH()), SwitchStatement.SwitchKeywordRole), SwitchStatement.SwitchKeywordRole);
             result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            var ex = context.expression();
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);
       
             result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
             result.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
       
            var sects = context.switch_section();
            if (sects != null)
            {
                foreach (var child in sects)
                {
                    SwitchSection newSection = new SwitchSection();
                    foreach (var lb in child.switch_label())
                    {
                        var swlb = lb.Accept(this);
                        var caseLabel = swlb as CaseLabel;
                        if (caseLabel != null)
                            newSection.AddChild(caseLabel, SwitchSection.CaseLabelRole);


                    }
                    var statement = child.statement_list();
                    foreach(var stm in statement.statement())
                             newSection.AddChild((Statement)stm.Accept(this), Roles.EmbeddedStatement);
                }
            }
      
           
            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
           

            return result;
        
        }
        public override AstNode VisitSwitch_label(RSharpParser.Switch_labelContext context)
        {
            var newLabel = new CaseLabel();
            var def = context.DEFAULT();

            if (def == null)
            {
                newLabel.AddChild(new CSharpTokenNode(Convert(context.CASE()), CaseLabel.CaseKeywordRole), CaseLabel.CaseKeywordRole);
                var ex = context.expression();
                if (ex != null)
                    newLabel.AddChild((Expression)ex.Accept(this), Roles.Expression);
                newLabel.AddChild(new CSharpTokenNode(Convert(context.COLON()), Roles.Colon), Roles.Colon);
            }
            else
            {
                newLabel.AddChild(new CSharpTokenNode(Convert(def), CaseLabel.DefaultKeywordRole), CaseLabel.DefaultKeywordRole);
                newLabel.AddChild(new CSharpTokenNode(Convert(context.COLON()), Roles.Colon), Roles.Colon);
            }
            return newLabel;
        }
        public override AstNode VisitCheckedStatement(RSharpParser.CheckedStatementContext context)
        {
            var result = new CheckedStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.CHECKED()), CheckedStatement.CheckedKeywordRole), CheckedStatement.CheckedKeywordRole);

                var convBlock = context.block().Accept(this) as BlockStatement;
                if (convBlock != null)
                    result.AddChild(convBlock, Roles.Body);
         
            return result;
        }
        public override AstNode VisitUncheckedStatement(RSharpParser.UncheckedStatementContext context)
        {
            var result = new UncheckedStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.UNCHECKED()), UncheckedStatement.UncheckedKeywordRole), UncheckedStatement.UncheckedKeywordRole);
         
                var convBlock = context.block().Accept(this) as BlockStatement;
                if (convBlock != null)
                    result.AddChild(convBlock, Roles.Body);
            
            return result;
        }
        public override AstNode VisitUnsafeStatement(RSharpParser.UnsafeStatementContext context)
        {
            var result = new UnsafeStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.UNSAFE()), UnsafeStatement.UnsafeKeywordRole), UnsafeStatement.UnsafeKeywordRole);

            var convBlock = context.block().Accept(this) as BlockStatement;
                if (convBlock != null)
                    result.AddChild(convBlock, Roles.Body);
          
            return result;
        }
        public override AstNode VisitForeachStatement(RSharpParser.ForeachStatementContext context)
        {
            var result = new ForeachStatement();

            result.AddChild(new CSharpTokenNode(Convert(context.FOREACH()), ForeachStatement.ForeachKeywordRole), ForeachStatement.ForeachKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);

                var tp = context.type();
            if (tp != null)
                result.AddChild(ConvertToType(tp), Roles.Type);

            var v = context.identifier();
            var ex = context.expression();
            var stmt = context.embedded_statement();
            if (v != null)
                result.AddChild(Identifier.Create(v.GetText(), Convert(v.start)), Roles.Identifier);

          
                result.AddChild(new CSharpTokenNode(Convert(context.IN()), ForeachStatement.InKeywordRole), ForeachStatement.InKeywordRole);

            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);

                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

            if (stmt != null)
                result.AddChild((Statement)stmt.Accept(this), Roles.EmbeddedStatement);

            return result;
        }
        public override AstNode VisitForStatement(RSharpParser.ForStatementContext context)
        {
            var result = new ForStatement();
            var fori = context.for_initializer();
            var forit = context.for_iterator();
       
            
            result.AddChild(new CSharpTokenNode(Convert(context.FOR()), ForStatement.ForKeywordRole), ForStatement.ForKeywordRole);
              result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
        if(fori != null)
             {
                 var exprs = fori.expression();
                 var lvar = fori.local_variable_declaration();
            if (lvar != null)
                result.AddChild((Statement)lvar.Accept(this), ForStatement.InitializerRole);
            else
            {
                foreach(var ex in exprs)
                    result.AddChild((Statement)CreateExpressionStatement(ex), ForStatement.InitializerRole);
            }

           }
         
            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON(0)), Roles.Semicolon), Roles.Semicolon);
            var expr = context.expression();
            if (expr != null)
                result.AddChild((Expression)expr.Accept(this), Roles.Condition);

            result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON(0)), Roles.Semicolon), Roles.Semicolon);

            if (fori != null)
            {
                var exprs = fori.expression();
                foreach (var ex in exprs)
                    result.AddChild((Statement)CreateExpressionStatement(ex), ForStatement.IteratorRole);
            }
       
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

   
                result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);

            return result;
        }
        public override AstNode VisitFor_initializer(RSharpParser.For_initializerContext context)
        {
          
            // handled by for
            throw new NotSupportedException();
        }
        public override AstNode VisitFor_iterator(RSharpParser.For_iteratorContext context)
        {
            // handled by for
            throw new NotSupportedException();
        }
        public override AstNode VisitSyncStatement(RSharpParser.SyncStatementContext context)
        {
            var result = new LockStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.SYNC()), LockStatement.LockKeywordRole), LockStatement.LockKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            var ex = context.expression();
          
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);

 
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
 
                result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);

            return result;
        }
        public override AstNode VisitRestrictStatement(RSharpParser.RestrictStatementContext context)
        {
            var result = new RestrictStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.RESTRICT()), LockStatement.LockKeywordRole), LockStatement.LockKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            var ex = context.expression();

            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Expression);


            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

            result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);

            return result;
        }
        public override AstNode VisitWhileStatement(RSharpParser.WhileStatementContext context)
        {
            var result = new WhileStatement();
         
            result.AddChild(new CSharpTokenNode(Convert(context.WHILE()), WhileStatement.WhileKeywordRole), WhileStatement.WhileKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                var ex = context.expression();
                if (ex != null)
                    result.AddChild((Expression)ex.Accept(this), Roles.Condition);
          
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
                result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);
            return result;
        }
        public override AstNode VisitWhileElseStatement(RSharpParser.WhileElseStatementContext context)
        {
            var result = new WhileElseStatement();

            result.AddChild(new CSharpTokenNode(Convert(context.WHILE()), WhileElseStatement.WhileKeywordRole), WhileElseStatement.WhileKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            var ex = context.expression();
            if (ex != null)
                result.AddChild((Expression)ex.Accept(this), Roles.Condition);

            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            result.AddChild((Statement)context.embedded_statement(0).Accept(this), Roles.EmbeddedStatement);
            result.AddChild(new CSharpTokenNode(Convert(context.ELSE()), WhileElseStatement.ElseKeywordRole), WhileElseStatement.ElseKeywordRole);
            result.AddChild((Statement)context.embedded_statement(1).Accept(this), Roles.ElseEmbeddedStatement);
            return result;
        }
        public override AstNode VisitUsingStatement(RSharpParser.UsingStatementContext context)
        {
            var result = new UsingStatement();
        
            result.AddChild(new CSharpTokenNode(Convert(context.USING()), UsingStatement.UsingKeywordRole), UsingStatement.UsingKeywordRole);

                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                var ra = context.resource_acquisition();
                if (ra != null)
                   result.AddChild(ra.Accept(this), UsingStatement.ResourceAcquisitionRole);
               
       
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

          
                result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);
            return result;
        }
        public override AstNode VisitResource_acquisition(RSharpParser.Resource_acquisitionContext context)
        {
            var expr = context.expression();
            var lvar = context.local_variable_declaration();
            if (expr != null)
                return expr.Accept(this);
            else return lvar.Accept(this);
          
        }
        public override AstNode VisitDoStatement(RSharpParser.DoStatementContext context)
        {
            var result = new DoWhileStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.DO()), DoWhileStatement.DoKeywordRole), DoWhileStatement.DoKeywordRole);

                result.AddChild((Statement)context.embedded_statement().Accept(this), Roles.EmbeddedStatement);
                result.AddChild(new CSharpTokenNode(Convert(context.WHILE()), DoWhileStatement.WhileKeywordRole), DoWhileStatement.WhileKeywordRole);
       
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
          
                result.AddChild((Expression)context.expression().Accept(this), Roles.Condition);
        
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            
              result.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
           

            return result;
        }
        public override AstNode VisitTryCatchStatement(RSharpParser.TryCatchStatementContext context)
        {
            var result = new TryCatchStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.TRY()), TryCatchStatement.TryKeywordRole), TryCatchStatement.TryKeywordRole);
         
                result.AddChild((BlockStatement)context.block().Accept(this), TryCatchStatement.TryBlockRole);
                var ca = context.catch_clauses();
            if (ca != null)
            {
                var gcc = ca.general_catch_clause();
                if (gcc == null)
                {
                    foreach (var ctch in ca.specific_catch_clause())
                        result.AddChild((CatchClause)ctch.Accept(this), TryCatchStatement.CatchClauseRole);

                }
                else result.AddChild((CatchClause)gcc.Accept(this), TryCatchStatement.CatchClauseRole);
            }

            var fin = context.finally_clause();
            if (fin != null)
            {
                result.AddChild(new CSharpTokenNode(Convert(fin.FINALLY()), TryCatchStatement.FinallyKeywordRole), TryCatchStatement.FinallyKeywordRole);
                result.AddChild((BlockStatement)fin.block().Accept(this), TryCatchStatement.FinallyBlockRole);
            }
    
            return result;
        }
        public override AstNode VisitSpecific_catch_clause(RSharpParser.Specific_catch_clauseContext context)
        {
            var result = new CatchClause();
            result.AddChild(new CSharpTokenNode(Convert(context.CATCH()), CatchClause.CatchKeywordRole), CatchClause.CatchKeywordRole);
            var tp = context.package_or_type_name();
            var ident = context.identifier();
            var ef = context.exception_filter();
            if (tp != null)
            {
   
                     result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                    result.AddChild(ConvertToType(tp), Roles.Type);
              
                if(ident != null)
                    result.AddChild(Identifier.Create(ident.GetText(), Convert(ident.start)), Roles.Identifier);

           
                    result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

            }

            if (ef != null)
            {
                result.AddChild(new CSharpTokenNode(Convert(ef.WHEN()), CatchClause.WhenKeywordRole), CatchClause.WhenKeywordRole);
                result.AddChild((Expression)ef.expression().Accept(this), CatchClause.ConditionRole);
            }


            result.AddChild((BlockStatement)context.block().Accept(this), Roles.Body);

            return result;
        }
        public override AstNode VisitException_filter(RSharpParser.Exception_filterContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitGeneral_catch_clause(RSharpParser.General_catch_clauseContext context)
        {
            var result = new CatchClause();
            result.AddChild(new CSharpTokenNode(Convert(context.CATCH()), CatchClause.CatchKeywordRole), CatchClause.CatchKeywordRole);     
            var ef = context.exception_filter();
          

            if (ef != null)
            {
                result.AddChild(new CSharpTokenNode(Convert(ef.WHEN()), CatchClause.WhenKeywordRole), CatchClause.WhenKeywordRole);
                result.AddChild((Expression)ef.expression().Accept(this), CatchClause.ConditionRole);
            }


            result.AddChild((BlockStatement)context.block().Accept(this), Roles.Body);

     
            return result;
     
        }
        public override AstNode VisitTryFinallyStatement(RSharpParser.TryFinallyStatementContext context)
        {
              var result = new TryCatchStatement();
            result.AddChild(new CSharpTokenNode(Convert(context.TRY()), TryCatchStatement.TryKeywordRole), TryCatchStatement.TryKeywordRole);
            result.AddChild((BlockStatement)context.block().Accept(this), TryCatchStatement.TryBlockRole);
              
            var fin = context.finally_clause();
            if (fin != null){
                result.AddChild(new CSharpTokenNode(Convert(fin.FINALLY()), TryCatchStatement.FinallyKeywordRole), TryCatchStatement.FinallyKeywordRole);
                result.AddChild((BlockStatement)fin.block().Accept(this), TryCatchStatement.FinallyBlockRole);
            }

            return result;
        }
        public override AstNode VisitFinally_clause(RSharpParser.Finally_clauseContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitCatch_clauses(RSharpParser.Catch_clausesContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitLocal_constant_declaration(RSharpParser.Local_constant_declarationContext context)
        {
            var result = new VariableDeclarationStatement();
            result.AddChild(new CSharpModifierToken(Convert(context.CONST()), Modifiers.Const), VariableDeclarationStatement.ModifierRole);
            result.AddChild(ConvertToType(context.type()), Roles.Type);
            var decls = context.constant_declarators();
            if (decls != null)
            {
                int i = 0;
                foreach (var decl in decls.constant_declarator())
                {
                    var init = new VariableInitializer();
                    var id = decl.identifier();
                    init.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                    var initializer = decl.expression();
                    if (initializer != null)
                    {
                        init.AddChild(new CSharpTokenNode(Convert(decl.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                        init.AddChild((Expression)initializer.Accept(this), Roles.Expression);
                    }
                    result.AddChild(init, Roles.Variable);

                    var comma = decls.COMMA(i);
                    if (comma != null)
                    {
                        result.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        i++;
                    }
                }
            }
            result.AddChild(new CSharpTokenNode(Convert(context.stop), Roles.Semicolon), Roles.Semicolon);
            return result;

        }
        public override AstNode VisitLocal_variable_declaration(RSharpParser.Local_variable_declarationContext context)
        {
            var result = new VariableDeclarationStatement();
            result.AddChild(ConvertToType(context.type()), Roles.Type);
            var decls = context.local_variable_declarator();
            if (decls != null)
            {
                int i = 0;
                foreach (var decl in decls)
                {
                    var init = new VariableInitializer();
                    var id = decl.identifier();
                    init.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                    var initializer = decl.variable_initializer();
                    if (initializer != null)
                    {
                        init.AddChild(new CSharpTokenNode(Convert(decl.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                        init.AddChild((Expression)initializer.Accept(this), Roles.Expression);
                    }
                    result.AddChild(init, Roles.Variable);

                    var comma = context.COMMA(i);
                    if (comma != null)
                    {
                        result.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        i++;
                    }
                }
            }
            result.AddChild(new CSharpTokenNode(Convert(context.stop), Roles.Semicolon), Roles.Semicolon);
            return result;
        }
        public override AstNode VisitDeclarationStatement(RSharpParser.DeclarationStatementContext context)
        {
            return base.VisitDeclarationStatement(context);
        }
        public override AstNode VisitPackage_declaration(RSharpParser.Package_declarationContext context)
        {

            NamespaceDeclaration nDecl = null;
            string name = context.qualified_identifier().GetText();
            var pb = context.package_body();
  
                nDecl = new NamespaceDeclaration();
                nDecl.AddChild(new CSharpTokenNode(Convert(context.PACKAGE().Symbol), Roles.NamespaceKeyword), Roles.NamespaceKeyword);
                nDecl.AddChild(ConvertNamespaceName(context.qualified_identifier()), NamespaceDeclaration.NamespaceNameRole);

                nDecl.AddChild(new CSharpTokenNode(Convert(pb.OPEN_BRACE().Symbol), Roles.LBrace), Roles.LBrace);
                AddToNamespace(nDecl);
                namespaceStack.Push(nDecl);
   
            // imports
         var  imports = pb.import_directives();
            if (imports != null)
            {
                foreach (var us in imports.import_directive())
                    us.Accept(this);
            }
            // subcontainers
            var cont = pb.package_member_declarations();
            if (cont != null)
            {
                foreach (var subContainer in cont.package_member_declaration())
                    subContainer.Accept(this);

            }
            nDecl.AddChild(new CSharpTokenNode(Convert(pb.CLOSE_BRACE().Symbol), Roles.RBrace), Roles.RBrace);
            namespaceStack.Pop();

            return nDecl;
        }



        public override AstNode VisitUnary_expression(RSharpParser.Unary_expressionContext context)
        {
            var result = new UnaryOperatorExpression();
            var pl = context.PLUS();
            var mi = context.MINUS();
            var bang = context.BANG();
            var tilde = context.TILDE();
            var amp = context.AMP();
            var dec = context.OP_DEC();
            var inc = context.OP_INC();
            var par = context.OP_PARITY();
            var zero = context.OP_ZERO();
            var st = context.STAR();
            var uol = context.UNARY_OPERATOR_LITERAL();
            result.Operator = UnaryOperatorType.Any;

            if (pl != null)
                result.Operator = UnaryOperatorType.Plus;
            else   if (mi != null)
                result.Operator = UnaryOperatorType.Minus;
            else   if (bang != null)
                result.Operator = UnaryOperatorType.BitNot;
            else   if (tilde != null)
                result.Operator = UnaryOperatorType.Not;
            else   if (amp != null)
                result.Operator = UnaryOperatorType.AddressOf;
            else   if (dec != null)
                result.Operator = UnaryOperatorType.Decrement;
            else   if (inc != null)
                result.Operator = UnaryOperatorType.Increment;
            else   if (par != null)
                result.Operator = UnaryOperatorType.IsEven;
            else   if (zero != null)
                result.Operator = UnaryOperatorType.IsZero;
            else  if (st != null)
                result.Operator = UnaryOperatorType.Dereference;
            else  if (uol != null)
                result.Operator = UnaryOperatorType.CustomOperator;

            var prim = context.primary_expression();
            if (result.Operator != UnaryOperatorType.Any)
            {
                var r = UnaryOperatorExpression.GetOperatorRole(result.Operator);
                var expr = context.unary_expression();
                result.AddChild(new CSharpTokenNode(Convert(expr.start), r), r);
                result.AddChild((Expression)expr.Accept(this), Roles.Expression);
                return result;
            }
            else if (prim != null)
                return prim.Accept(this);
            else
            {
                var cresult = new CastExpression();


                cresult.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                cresult.AddChild(ConvertToType(context.type()), Roles.Type);

                cresult.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
                var expr = context.unary_expression();
                cresult.AddChild((Expression)expr.Accept(this), Roles.Expression);
                return cresult;
            }
        


          
        }
        public override AstNode VisitTypeofExpression(RSharpParser.TypeofExpressionContext context)
        {
            var result = new TypeOfExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.TYPEOF()), TypeOfExpression.TypeofKeywordRole), TypeOfExpression.TypeofKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                result.AddChild(ConvertToType(context.type()), Roles.Type);
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitSizeofExpression(RSharpParser.SizeofExpressionContext context)
        {
            var result = new SizeOfExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.SIZEOF()), SizeOfExpression.SizeofKeywordRole), SizeOfExpression.SizeofKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                result.AddChild(ConvertToType(context.type()), Roles.Type);
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitCheckedExpression(RSharpParser.CheckedExpressionContext context)
        {
            var result = new CheckedExpression();
  
            result.AddChild(new CSharpTokenNode(Convert(context.CHECKED()), CheckedExpression.CheckedKeywordRole), CheckedExpression.CheckedKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                result.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
                result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitUncheckedExpression(RSharpParser.UncheckedExpressionContext context)
        {
            var result = new UncheckedExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.UNCHECKED()), CheckedExpression.CheckedKeywordRole), CheckedExpression.CheckedKeywordRole);
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            result.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitDefaultValueExpression(RSharpParser.DefaultValueExpressionContext context)
        {
            var result = new DefaultValueExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.DEFAULT()), DefaultValueExpression.DefaultKeywordRole), DefaultValueExpression.DefaultKeywordRole);
       result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            result.AddChild(ConvertToType(context.type()), Roles.Type);
        result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitSuperAccessExpression(RSharpParser.SuperAccessExpressionContext context)
        {
            var result = new BaseReferenceExpression();
            result.Location = Convert(context.start);
            return result;
        }
        public override AstNode VisitSelfReferenceExpression(RSharpParser.SelfReferenceExpressionContext context)
        {

            var result = new ThisReferenceExpression();
            result.Location = Convert(context.start);
            return result;
        }
        public override AstNode VisitParenthesisExpressions(RSharpParser.ParenthesisExpressionsContext context)
        {
            var result = new ParenthesizedExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            result.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitSimpleNameExpression(RSharpParser.SimpleNameExpressionContext context)
        {
            return context.simple_name().Accept(this);
        }
        public override AstNode VisitSimple_name(RSharpParser.Simple_nameContext context)
        {
            var result = new IdentifierExpression();
            var id = context.identifier();
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            AddTypeArguments(context.type_argument_list(), result);
            return result;
        }
        public override AstNode VisitSimple_name_access(RSharpParser.Simple_name_accessContext context)
        {
            var result = new IdentifierExpression();
            var id = context.identifier();
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            AddTypeArguments(context.type_argument_list(), result);
            return result;
        }
        public override AstNode VisitPredefined_type(RSharpParser.Predefined_typeContext context)
        {
            var result = new IdentifierExpression();
            result.AddChild(Identifier.Create(context.GetText(), Convert(context.start)), Roles.Identifier);
            return result;
        }
        public override AstNode VisitQualified_alias_member(RSharpParser.Qualified_alias_memberContext context)
        {
            return new TypeReferenceExpression { Type = ConvertToType(context) };
        }
        public override AstNode VisitQualified_identifier(RSharpParser.Qualified_identifierContext context)
        {
            return new TypeReferenceExpression { Type = ConvertToType(context) };
        }

        #region Binary Operators
        public override AstNode VisitIsExpression(RSharpParser.IsExpressionContext context)
        {
            var result = new IsExpression();
           result.AddChild((Expression)context.relational_expression().Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.IS()), IsExpression.IsKeywordRole), IsExpression.IsKeywordRole);
                result.AddChild(ConvertToType(context.type()), Roles.Type);
            return result;
        }
        public override AstNode VisitAsExpression(RSharpParser.AsExpressionContext context)
        {
            var result = new AsExpression();
            result.AddChild((Expression)context.relational_expression().Accept(this), Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.AS()), AsExpression.AsKeywordRole), AsExpression.AsKeywordRole);
           result.AddChild(ConvertToType(context.type()), Roles.Type);
            return result;
        }
        public override AstNode VisitConditional_and_expression(RSharpParser.Conditional_and_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var op = context.OP_AND();
            var left = context.conditional_and_expression();
            var right = context.inclusive_or_expression();
            if (op != null)
            {
                result.Operator = BinaryOperatorType.ConditionalAnd;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(op), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);

        }
        public override AstNode VisitConditional_or_expression(RSharpParser.Conditional_or_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var op = context.OP_OR();
            var left = context.conditional_or_expression();
            var right = context.conditional_and_expression();
            if (op != null)
            {
                result.Operator = BinaryOperatorType.ConditionalOr;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(op), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }
        public override AstNode VisitShift_expression(RSharpParser.Shift_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var lsop = context.OP_LEFT_SHIFT();
            var rsop = context.OP_RIGHT_SHIFT();
            var rrop = context.OP_RIGHT_ROTATE();
            var lrop = context.OP_LEFT_ROTATE();
            var left = context.shift_expression();
            var right = context.additive_expression();
            if (lsop != null)
            {
                result.Operator = BinaryOperatorType.ShiftLeft;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(lsop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (rsop != null)
            {
                result.Operator = BinaryOperatorType.ShiftRight;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(rsop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (rrop != null)
            {
                result.Operator = BinaryOperatorType.RotateRight;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(rrop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (lrop != null)
            {
                result.Operator = BinaryOperatorType.RotateLeft;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(lrop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }
        public override AstNode VisitAdditive_expression(RSharpParser.Additive_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.PLUS();
            var mop = context.MINUS();

            var left = context.additive_expression();
            var right = context.multiplicative_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.Add;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (mop != null)
            {
                result.Operator = BinaryOperatorType.Subtract;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(mop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }
        public override AstNode VisitMultiplicative_expression(RSharpParser.Multiplicative_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var sop = context.STAR();
            var dop = context.DIV();
            var mop = context.PERCENT();
            var left = context.multiplicative_expression();
            var right = context.unary_expression();
            if (sop != null)
            {
                result.Operator = BinaryOperatorType.Multiply;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(sop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (mop != null)
            {
                result.Operator = BinaryOperatorType.Modulus;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(mop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (dop != null)
            {
                result.Operator = BinaryOperatorType.Divide;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(dop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }  
        public override AstNode VisitRelationalExpression(RSharpParser.RelationalExpressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var gtop = context.GT();
            var ltop = context.LT();
            var geop = context.OP_GE();
            var leop = context.OP_LE();
            var left = context.relational_expression();
            var right = context.shift_expression();
            if (gtop != null)
            {
                result.Operator = BinaryOperatorType.GreaterThan;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(gtop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (ltop != null)
            {
                result.Operator = BinaryOperatorType.LessThan;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(ltop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (geop != null)
            {
                result.Operator = BinaryOperatorType.GreaterThanOrEqual;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(geop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (leop != null)
            {
                result.Operator = BinaryOperatorType.LessThanOrEqual;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(leop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }
        public override AstNode VisitEquality_expression(RSharpParser.Equality_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var eqop = context.OP_EQ();
            var neqop = context.OP_NE();

            var left = context.equality_expression();
            var right = context.relational_expression();
            if (eqop != null)
            {
                result.Operator = BinaryOperatorType.Equality;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(eqop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else if (neqop != null)
            {
                result.Operator = BinaryOperatorType.InEquality;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(neqop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
            else return right.Accept(this);
        }
        public override AstNode VisitInclusive_or_expression(RSharpParser.Inclusive_or_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.BITWISE_OR();
       

            var left = context.inclusive_or_expression();
            var right = context.exclusive_or_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.BitwiseOr;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }
      
            else return right.Accept(this);
        }
        public override AstNode VisitExclusive_or_expression(RSharpParser.Exclusive_or_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.CARRET();


            var left = context.exclusive_or_expression();
            var right = context.and_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.ExclusiveOr;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }

            else return right.Accept(this);
        }
        public override AstNode VisitAnd_expression(RSharpParser.And_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.AMP();


            var left = context.and_expression();
            var right = context.equality_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.BitwiseAnd;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }

            else return right.Accept(this);
        }
        public override AstNode VisitNull_coalescing_expression(RSharpParser.Null_coalescing_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.OP_COALESCING();


            var left = context.null_coalescing_expression();
            var right = context.binary_operator_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.NullCoalescing;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }

            else return right.Accept(this);
        }
        public override AstNode VisitBinary_operator_expression(RSharpParser.Binary_operator_expressionContext context)
        {
            var result = new BinaryOperatorExpression();
            var pop = context.BINARY_OPERATOR_LITERAL();


            var left = context.binary_operator_expression();
            var right = context.conditional_or_expression();
            if (pop != null)
            {
                result.Operator = BinaryOperatorType.CustomBinary;
                result.AddChild((Expression)left.Accept(this), BinaryOperatorExpression.LeftRole);
                var r = BinaryOperatorExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(pop), r), r);
                result.AddChild((Expression)right.Accept(this), BinaryOperatorExpression.RightRole);
                return result;
            }

            else return right.Accept(this);
        }
        public override AstNode VisitConditional_expression(RSharpParser.Conditional_expressionContext context)
        {

            var interr = context.INTERR();
            if (interr != null)
            {
                var result = new ConditionalExpression();
                result.AddChild((Expression)context.null_coalescing_expression().Accept(this), Roles.Condition);
                result.AddChild(new CSharpTokenNode(Convert(interr), ConditionalExpression.QuestionMarkRole), ConditionalExpression.QuestionMarkRole);
                result.AddChild((Expression)context.expression().Accept(this), ConditionalExpression.TrueRole);
                result.AddChild(new CSharpTokenNode(Convert(context.COLON()), ConditionalExpression.ColonRole), ConditionalExpression.ColonRole);
                result.AddChild((Expression)context.conditional_expression().Accept(this), ConditionalExpression.FalseRole);
                return result;
            }
            else return context.null_coalescing_expression().Accept(this);
        }
        #endregion

        public override AstNode VisitAssignment(RSharpParser.AssignmentContext context)
        {
            var result = new AssignmentExpression();
            var left = context.unary_expression();
            var right = context.expression();
            var aop = context.assignment_operator();
            result.Operator = AssignmentOperatorType.Any;

            var ass = aop.ASSIGNMENT();
            var add = aop.OP_ADD_ASSIGNMENT();
            var and = aop.OP_AND_ASSIGNMENT();
            var div = aop.OP_DIV_ASSIGNMENT();
            var lr = aop.OP_LEFT_ROTATE_ASSIGNMENT();
            var ls = aop.OP_LEFT_SHIFT_ASSIGNMENT();
            var mod = aop.OP_MOD_ASSIGNMENT();
            var mult = aop.OP_MULT_ASSIGNMENT();
            var or = aop.OP_OR_ASSIGNMENT();
            var rr = aop.OP_RIGHT_ROTATE_ASSIGNMENT();
            var rs = aop.OP_RIGHT_SHIFT_ASSIGNMENT();
            var sub = aop.OP_SUB_ASSIGNMENT();
            var xor = aop.OP_XOR_ASSIGNMENT();


            if (ass != null)
                result.Operator = AssignmentOperatorType.Assign;
            else if (add != null)
                result.Operator = AssignmentOperatorType.Add;
            else if (and != null)
                result.Operator = AssignmentOperatorType.BitwiseAnd;
            else if (div != null)
                result.Operator = AssignmentOperatorType.Divide;
            else if (lr != null)
                result.Operator = AssignmentOperatorType.RotateLeft;
            else if (ls != null)
                result.Operator = AssignmentOperatorType.ShiftLeft;
            else if (mod != null)
                result.Operator = AssignmentOperatorType.Modulus;
            else if (mult != null)
                result.Operator = AssignmentOperatorType.Multiply;
            else if (or != null)
                result.Operator = AssignmentOperatorType.BitwiseOr;
            else if (rr != null)
                result.Operator = AssignmentOperatorType.RotateRight;
            else if (rs != null)
                result.Operator = AssignmentOperatorType.ShiftRight;
            else if (sub != null)
                result.Operator = AssignmentOperatorType.Subtract;
            else if (xor != null)
                result.Operator = AssignmentOperatorType.ExclusiveOr;

                result.AddChild((Expression)left.Accept(this), AssignmentExpression.LeftRole);
                var r = AssignmentExpression.GetOperatorRole(result.Operator);
                result.AddChild(new CSharpTokenNode(Convert(context.assignment_operator().start), r), r);
                result.AddChild((Expression)right.Accept(this), AssignmentExpression.RightRole);
            return result;
        }
        public override AstNode VisitAssignment_operator(RSharpParser.Assignment_operatorContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitElement_access_expression(RSharpParser.Element_access_expressionContext context)
        {
           var left = (Expression)context.primary_expression_start().Accept(this);
           return AddBracketExpressions(left, context);
        }
        public override AstNode VisitAnonymousTypeExpression(RSharpParser.AnonymousTypeExpressionContext context)
        {
            var result = new AnonymousTypeCreateExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.NEW()), ObjectCreateExpression.NewKeywordRole), ObjectCreateExpression.NewKeywordRole);
                result.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
                foreach (var par in context.member_declarator())
                {
                    if (par == null)
                        continue;
                    result.AddChild((Expression)par.Accept(this), Roles.Expression);
     
                }
            result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
            return result;
        }
        public override AstNode VisitMember_declarator(RSharpParser.Member_declaratorContext context)
        {
            var id = context.identifier();
            if (id == null)
                return context.primary_expression().Accept(this);
            else
            {
                var namedExpression = new NamedExpression();
                namedExpression.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                namedExpression.AddChild(new CSharpTokenNode(Convert(context.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                namedExpression.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
                    return namedExpression;
            }
        }
        public override AstNode VisitObjectOrDelegateCreationExpression(RSharpParser.ObjectOrDelegateCreationExpressionContext context)
        {
            var result = new ObjectCreateExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.NEW()), ObjectCreateExpression.NewKeywordRole), ObjectCreateExpression.NewKeywordRole);
            result.AddChild(ConvertToType(context.type()), Roles.Type);
            var obj = context.object_creation_expression();
                result.AddChild(new CSharpTokenNode(Convert(obj.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                AddArguments(result, obj.argument_list());
                result.AddChild(new CSharpTokenNode(Convert(obj.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
           
      
            return result;
        }
        public override AstNode VisitAnonymousMethodExpression(RSharpParser.AnonymousMethodExpressionContext context)
        {
            var result = new AnonymousMethodExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.DELEGATE()), AnonymousMethodExpression.DelegateKeywordRole), AnonymousMethodExpression.DelegateKeywordRole);
                    result.HasParameterList = true;
                    result.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                    AddParameters(result, context.explicit_anonymous_function_parameter_list());
                    result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            
          
                var blockStatement = context.block().Accept(this) as BlockStatement;
                result.AddChild(blockStatement, Roles.Body);
           
            return result;
        }
        public override AstNode VisitExplicit_anonymous_function_parameter_list(RSharpParser.Explicit_anonymous_function_parameter_listContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitExplicit_anonymous_function_parameter(RSharpParser.Explicit_anonymous_function_parameterContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitLambda_expression(RSharpParser.Lambda_expressionContext context)
        {
            var result = new LambdaExpression();
            var afs = context.anonymous_function_signature();
            var id = afs.identifier();
            if (id != null)
                    AddParameters(result, id);
            else
            {
                var ifp = afs.implicit_anonymous_function_parameter_list();
                var efp = afs.explicit_anonymous_function_parameter_list();
                result.AddChild(new CSharpTokenNode(Convert(afs.OPEN_PARENS()), Roles.LPar), Roles.LPar);
              
                if(ifp != null)    
                    AddParameters(result,ifp);
                else if (efp != null)
                    AddParameters(result, efp);

                result.AddChild(new CSharpTokenNode(Convert(afs.CLOSE_PARENS()), Roles.RPar), Roles.RPar);  
            }
            result.AddChild(new CSharpTokenNode(Convert(context.RIGHT_ARROW()), LambdaExpression.ArrowRole), LambdaExpression.ArrowRole);

            var afb = context.anonymous_function_body();
            if (afb != null)
            {
                var blk = afb.block();
                if (blk != null)
                    result.AddChild((AstNode)blk.Accept(this), LambdaExpression.BodyRole);
                else
                    result.AddChild(CreateExpressionStatement(afb.expression()), LambdaExpression.BodyRole);
            }
            return result;
        }
        public override AstNode VisitAnonymous_function_signature(RSharpParser.Anonymous_function_signatureContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitAnonymous_function_body(RSharpParser.Anonymous_function_bodyContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitImplicit_anonymous_function_parameter_list(RSharpParser.Implicit_anonymous_function_parameter_listContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitArrayCreationExpression(RSharpParser.ArrayCreationExpressionContext context)
        {
            var result = new ArrayCreateExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.NEW()), ArrayCreateExpression.NewKeywordRole), ArrayCreateExpression.NewKeywordRole);
            var tp = context.type();
            if (tp != null)
                result.AddChild(ConvertToType(tp), Roles.Type);

            var exlist = context.expression_list();

            if (exlist != null)
            {
            
               result.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACKET()), Roles.LBracket), Roles.LBracket);
               var exprs = exlist.expression();
               for (int i = 0; i < exprs.Length; i++)
                {
                    var arg = exprs[i];
                    if (arg != null)
                        result.AddChild((Expression)arg.Accept(this), Roles.Argument);

                    var comma = exlist.COMMA(i);
                    if (comma != null)
                        result.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                }

              result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACKET()), Roles.RBracket), Roles.RBracket);

            }


            var ranks = context.rank_specifier();
            if (ranks != null)
            {
                foreach (var rank in ranks)
                    result.AddChild((ArraySpecifier)rank.Accept(this), ArrayCreateExpression.AdditionalArraySpecifierRole);
                
            }
         
            var init = context.array_initializer();
            if (init != null)
                result.AddChild((ArrayInitializerExpression)init.Accept(this), ArrayCreateExpression.InitializerRole);
      

            return result;
        }
        public override AstNode VisitArray_initializer(RSharpParser.Array_initializerContext context)
        {
            var result = new ArrayInitializerExpression();
            result.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            var inits = context.variable_initializer();
            for (int i = 0; i < inits.Length; i++)
            {
                var init = inits[i];
                if (init == null)
                    continue;
                result.AddChild((Expression)init.Accept(this), Roles.Expression);
                var comma = context.COMMA(i);
                if (comma != null)
                    result.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
            }


           result.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
     
            return result;
        }
        public override AstNode VisitVariable_initializer(RSharpParser.Variable_initializerContext context)
        {
            ParserRuleContext child = context.expression();
            if (child != null)
                return child.Accept(this);
            else return context.array_initializer().Accept(this);
        }
        public override AstNode VisitExpression_list(RSharpParser.Expression_listContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitRank_specifier(RSharpParser.Rank_specifierContext context)
        {
            var spec = new ArraySpecifier();
            spec.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACKET()), Roles.LBracket), Roles.LBracket);
            var c = context.COMMA();
            if (c != null)
            {
                foreach (var coma in c)
                    spec.AddChild(new CSharpTokenNode(Convert(coma), Roles.Comma), Roles.Comma);
            }
            spec.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACKET()), Roles.RBracket), Roles.RBracket);
            return spec;
        }
        Expression currentPrimaryExpression = null;
        public override AstNode VisitPrimary_expression(RSharpParser.Primary_expressionContext context)
        {
       
            var pes = context.primary_expression_start();
            if (pes != null)
                currentPrimaryExpression= (Expression)pes.Accept(this);
            else
            {
                var eae = context.element_access_expression();
                currentPrimaryExpression = (Expression)eae.Accept(this);
            }

            var pend = context.primary_expression_end();
            if (pend != null)
            {
                foreach (var pexpr in pend)
                    currentPrimaryExpression = (Expression)pexpr.Accept(this);
            }
          
            return currentPrimaryExpression;
        }
        public override AstNode VisitInvocationExpression(RSharpParser.InvocationExpressionContext context)
        {
            var result = new InvocationExpression();
            result.AddChild(currentPrimaryExpression, Roles.TargetExpression);
            var mi = context.method_invocation();
            result.AddChild(new CSharpTokenNode(Convert(mi.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            AddArguments(result,mi.argument_list());
            result.AddChild(new CSharpTokenNode(Convert(mi.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            return result;
        }
        public override AstNode VisitMemberAccessExpression(RSharpParser.MemberAccessExpressionContext context)
        {

              var  result = new MemberReferenceExpression();
              result.AddChild(currentPrimaryExpression, Roles.TargetExpression);
              var ma = context.member_access();
              result.AddChild(new CSharpTokenNode(Convert(ma.DOT()), Roles.Dot), Roles.Dot);
              result.AddChild(Identifier.Create(ma.identifier().GetText(), Convert(ma.identifier().start)), Roles.Identifier);
              AddTypeArguments(ma.type_argument_list(), result);
            return result;
        }
        public override AstNode VisitPostIncrementExpression(RSharpParser.PostIncrementExpressionContext context)
        {
            var result = new UnaryOperatorExpression();

                    result.Operator = UnaryOperatorType.PostIncrement;
                    result.AddChild(currentPrimaryExpression, Roles.Expression);
                    result.AddChild(new CSharpTokenNode(Convert(context.OP_INC()), UnaryOperatorExpression.IncrementRole), UnaryOperatorExpression.IncrementRole);
 
            return result;
        }
        public override AstNode VisitPostDecrementExpression(RSharpParser.PostDecrementExpressionContext context)
        {
            var result = new UnaryOperatorExpression();

            result.Operator = UnaryOperatorType.PostDecrement;
            result.AddChild(currentPrimaryExpression, Roles.Expression);
            result.AddChild(new CSharpTokenNode(Convert(context.OP_DEC()), UnaryOperatorExpression.DecrementRole), UnaryOperatorExpression.DecrementRole);

            return result;
        }
        public override AstNode VisitPointerMemberAccessExpression(RSharpParser.PointerMemberAccessExpressionContext context)
        {
           var result = new PointerReferenceExpression();
            result.AddChild(currentPrimaryExpression, Roles.TargetExpression);
            result.AddChild(new CSharpTokenNode(Convert(context.OP_PTR()), PointerReferenceExpression.ArrowRole), PointerReferenceExpression.ArrowRole);
            var ma = context.identifier();
            result.AddChild(Identifier.Create(ma.GetText(), Convert(ma.start)), Roles.Identifier);
            AddTypeArguments(context.type_argument_list(), result);
            return result;
        }
        readonly Stack<TypeDeclaration> typeStack = new Stack<TypeDeclaration>();
       
        public override AstNode VisitProperty_declaration(RSharpParser.Property_declarationContext context)
        {
            var newProperty = new PropertyDeclaration();
            AddAttributeSection(newProperty, context.attributes());
            AddModifiers(newProperty, context.method_modifiers());
            newProperty.AddChild(ConvertToType(context.type()), Roles.Type);
           var mn = context.member_name();
            AddExplicitInterface(newProperty, mn);
            var ident = mn.identifier();
            newProperty.AddChild(Identifier.Create(ident.GetText(), Convert(ident.start)), Roles.Identifier);
            var rr = context.RIGHT_ARROW();
            if (rr == null)
            {
                newProperty.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
                var accdecl = context.accessor_declarations();
                var getacc = accdecl.get_accessor_declaration();
                var setacc = accdecl.set_accessor_declaration();
                Accessor getAccessor = null;
                if (getacc != null)
                {
                    getAccessor = new Accessor();
                    AddAttributeSection(getAccessor, getacc.attributes());
                    AddModifiers(getAccessor,context.method_modifiers());
                    getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.GET()), PropertyDeclaration.GetKeywordRole), PropertyDeclaration.GetKeywordRole);
                    var accb = getacc.accessor_body().Accept(this);
                    if (accb != null)
                    {
                        var convBlock = accb as BlockStatement;
                        if (convBlock != null)
                            getAccessor.AddChild(convBlock, Roles.Body);
                    }
                    else
                        getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.accessor_body().SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

                }

                Accessor setAccessor = null;
                if (setacc != null)
                {
                    setAccessor = new Accessor();
                    AddAttributeSection(setAccessor, setacc.attributes());
                    AddModifiers(setAccessor, setacc.accessor_modifier());
                    setAccessor.AddChild(new CSharpTokenNode(Convert(setacc.SET()), PropertyDeclaration.SetKeywordRole), PropertyDeclaration.SetKeywordRole);

                    var accb = setacc.accessor_body().Accept(this);
                    if (accb != null)
                    {
                        var convBlock = accb as BlockStatement;
                        if (convBlock != null)
                            setAccessor.AddChild(convBlock, Roles.Body);
                    }
                    else
                        setAccessor.AddChild(new CSharpTokenNode(Convert(setacc.accessor_body().SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

                }

                if (getAccessor != null && setAccessor != null)
                {
                    if (getAccessor.StartLocation < setAccessor.StartLocation)
                    {
                        newProperty.AddChild(getAccessor, PropertyDeclaration.GetterRole);
                        newProperty.AddChild(setAccessor, PropertyDeclaration.SetterRole);
                    }
                    else
                    {
                        newProperty.AddChild(setAccessor, PropertyDeclaration.SetterRole);
                        newProperty.AddChild(getAccessor, PropertyDeclaration.GetterRole);
                    }
                }
                else
                {
                    if (getAccessor != null)
                        newProperty.AddChild(getAccessor, PropertyDeclaration.GetterRole);

                    if (setAccessor != null)
                        newProperty.AddChild(setAccessor, PropertyDeclaration.SetterRole);
                }

                newProperty.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
          
            }
            else
            {
                var block = CreateArrowStatement(context.expression());
                //TODO:Compiler Generated block


                // var getAccessor = new Accessor();
                //AddAttributeSection(getAccessor, getacc.attributes());
                //AddModifiers(getAccessor, getacc.accessor_modifier());
                //getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.GET()), PropertyDeclaration.GetKeywordRole), PropertyDeclaration.GetKeywordRole);
                //var accb = getacc.accessor_body().Accept(this);
                //if (accb != null)
                //{
                //    var convBlock = accb as BlockStatement;
                //    if (convBlock != null)
                //        getAccessor.AddChild(convBlock, Roles.Body);
                //}
                //else
                //    getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.accessor_body().SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

            }
            typeStack.Peek().AddChild(newProperty, Roles.TypeMemberRole);
            return newProperty;
        }
        public override AstNode VisitAccessor_declarations(RSharpParser.Accessor_declarationsContext context)
        {
            throw new NotSupportedException();
        }
        public override AstNode VisitAccessor_body(RSharpParser.Accessor_bodyContext context)
        {
            var bl = context.block();
            return bl != null ? bl.Accept(this) :  null;
        }
        public override AstNode VisitConstructor_declaration(RSharpParser.Constructor_declarationContext context)
        {
            var newConstructor = new ConstructorDeclaration();
            AddAttributeSection(newConstructor,context.attributes());
            AddModifiers(newConstructor, context.method_modifiers());
          
            newConstructor.AddChild(Identifier.Create(typeStack.Peek().Name, Convert(context.SELF())), Roles.Identifier);
            newConstructor.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            AddParameters(newConstructor, context.formal_parameter_list());
            newConstructor.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

            var cin = context.constructor_initializer();
            if (cin != null)
            {
                    newConstructor.AddChild(new CSharpTokenNode(Convert(cin.COLON()), Roles.Colon), Roles.Colon);
                    newConstructor.AddChild((ConstructorInitializer)cin.Accept(this), ConstructorDeclaration.InitializerRole);

            }
            var body = context.body().Accept(this);
            if (body != null)
            {
                var blockStatement = body as BlockStatement;
                if (blockStatement != null)
                    newConstructor.AddChild(blockStatement, Roles.Body);
            }
            
            typeStack.Peek().AddChild(newConstructor, Roles.TypeMemberRole);

            return newConstructor;
        }
        public override AstNode VisitConstructor_initializer(RSharpParser.Constructor_initializerContext context)
        {
            var initializer = new ConstructorInitializer();
            var self = context.SELF();
            initializer.ConstructorInitializerType = self  == null ? ConstructorInitializerType.Base : ConstructorInitializerType.This;
                // this and base has the same length
                var r = initializer.ConstructorInitializerType == ConstructorInitializerType.This ? ConstructorInitializer.ThisKeywordRole : ConstructorInitializer.BaseKeywordRole;
            if(self != null)
               initializer.AddChild(new CSharpTokenNode(Convert(self), r), r);
            else      initializer.AddChild(new CSharpTokenNode(Convert(context.SUPER()), r), r);


                initializer.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
                AddArguments(initializer, context.argument_list());
                initializer.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

                return initializer;
        }
        public override AstNode VisitBody(RSharpParser.BodyContext context)
        {
            var bl = context.block();
        return bl != null ? bl.Accept(this) : null;
        }
        public override AstNode VisitDestructor_definition(RSharpParser.Destructor_definitionContext context)
        {
            var newDestructor = new DestructorDeclaration();
            AddAttributeSection(newDestructor, context.attributes());
            AddModifiers(newDestructor, context.method_modifiers());
            newDestructor.AddChild(new CSharpTokenNode(Convert(context.TILDE()), DestructorDeclaration.TildeRole), DestructorDeclaration.TildeRole);
            newDestructor.AddChild(Identifier.Create(typeStack.Peek().Name, Convert(context.SELF())), Roles.Identifier);
            newDestructor.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            newDestructor.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);

            var body = context.body().Accept(this);
            if (body != null)
            {
                var blockStatement = body as BlockStatement;
                if (blockStatement != null)
                    newDestructor.AddChild(blockStatement, Roles.Body);
            }
            typeStack.Peek().AddChild(newDestructor, Roles.TypeMemberRole);

            return newDestructor;
        }

        public override AstNode VisitMethod_declaration(RSharpParser.Method_declarationContext context)
        {
            var newMethod = new MethodDeclaration();
            AddAttributeSection(newMethod, context.attributes());
            AddModifiers(newMethod, context.method_modifiers());
            newMethod.AddChild(ConvertToType(context.return_type()), Roles.Type);
           // AddExplicitInterface(newMethod, m.MethodName);
            var mn = context.member_name();
            newMethod.AddChild(Identifier.Create(mn.identifier().GetText(), Convert(mn.start)), Roles.Identifier);
            AddTypeParameters(newMethod, context.type_parameter_list());
           newMethod.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            AddParameters(newMethod, context.formal_parameter_list());
            newMethod.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            AddConstraints(newMethod,context.type_parameter_constraints_clauses());
            var mbody = context.method_body().Accept(this);
            if (mbody != null)
            {
                var bodyBlock = mbody as BlockStatement;
               
                if (bodyBlock != null)
                    newMethod.AddChild(bodyBlock, Roles.Body);
                
            }
           
            typeStack.Peek().AddChild(newMethod, Roles.TypeMemberRole);

            return newMethod;
        }
        public override AstNode VisitMethod_body(RSharpParser.Method_bodyContext context)
        {
            var bl = context.block();
            if (bl != null)
                return bl.Accept(this);

            var rr = context.RIGHT_ARROW();
            if (rr != null)
                return CreateArrowStatement(context.expression());
            else return null;

        }
        public override AstNode VisitIndexer_declaration(RSharpParser.Indexer_declarationContext context)
        {
            var newIndexer = new IndexerDeclaration();
            AddAttributeSection(newIndexer, context.attributes());
            AddModifiers(newIndexer, context.method_modifiers());
            newIndexer.AddChild(ConvertToType(context.type()), Roles.Type);
            //AddExplicitInterface(newIndexer, i.MemberName);
            newIndexer.AddChild(new CSharpTokenNode(Convert(context.SELF()), IndexerDeclaration.ThisKeywordRole), IndexerDeclaration.ThisKeywordRole);
            newIndexer.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACKET()), Roles.LBracket), Roles.LBracket);
            AddParameters(newIndexer, context.formal_parameter_list());
            newIndexer.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACKET()), Roles.RBracket), Roles.RBracket);
     var rr = context.RIGHT_ARROW();
     if (rr == null)
     {
         newIndexer.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
         var accdecl = context.accessor_declarations();
         var getacc = accdecl.get_accessor_declaration();
         var setacc = accdecl.set_accessor_declaration();
         Accessor getAccessor = null;
         if (getacc != null)
         {
             getAccessor = new Accessor();
             AddAttributeSection(getAccessor, getacc.attributes());
             AddModifiers(getAccessor, context.method_modifiers());
             getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.GET()), PropertyDeclaration.GetKeywordRole), PropertyDeclaration.GetKeywordRole);
             var accb = getacc.accessor_body().Accept(this);
             if (accb != null)
             {
                 var convBlock = accb as BlockStatement;
                 if (convBlock != null)
                     getAccessor.AddChild(convBlock, Roles.Body);
             }
             else
                 getAccessor.AddChild(new CSharpTokenNode(Convert(getacc.accessor_body().SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

         }

         Accessor setAccessor = null;
         if (setacc != null)
         {
             setAccessor = new Accessor();
             AddAttributeSection(setAccessor, setacc.attributes());
             AddModifiers(setAccessor, setacc.accessor_modifier());
             setAccessor.AddChild(new CSharpTokenNode(Convert(setacc.SET()), PropertyDeclaration.SetKeywordRole), PropertyDeclaration.SetKeywordRole);

             var accb = setacc.accessor_body().Accept(this);
             if (accb != null)
             {
                 var convBlock = accb as BlockStatement;
                 if (convBlock != null)
                     setAccessor.AddChild(convBlock, Roles.Body);
             }
             else
                 setAccessor.AddChild(new CSharpTokenNode(Convert(setacc.accessor_body().SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

         }

         if (getAccessor != null && setAccessor != null)
         {
             if (getAccessor.StartLocation < setAccessor.StartLocation)
             {
                 newIndexer.AddChild(getAccessor, PropertyDeclaration.GetterRole);
                 newIndexer.AddChild(setAccessor, PropertyDeclaration.SetterRole);
             }
             else
             {
                 newIndexer.AddChild(setAccessor, PropertyDeclaration.SetterRole);
                 newIndexer.AddChild(getAccessor, PropertyDeclaration.GetterRole);
             }
         }
         else
         {
             if (getAccessor != null)
                 newIndexer.AddChild(getAccessor, PropertyDeclaration.GetterRole);

             if (setAccessor != null)
                 newIndexer.AddChild(setAccessor, PropertyDeclaration.SetterRole);
         }

         newIndexer.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
  
     }
     else
     {
         var block = CreateArrowStatement(context.expression());
         //TODO:Compiler Generated block
     }
         
            typeStack.Peek().AddChild(newIndexer, Roles.TypeMemberRole);
            return newIndexer;
        }
        public override AstNode VisitEvent_declaration(RSharpParser.Event_declarationContext context)
        {
            var newEvent = new CustomEventDeclaration();
            AddAttributeSection(newEvent, context.attributes());
            AddModifiers(newEvent, context.method_modifiers());
            newEvent.AddChild(new CSharpTokenNode(Convert(context.EVENT()), CustomEventDeclaration.EventKeywordRole), CustomEventDeclaration.EventKeywordRole);
            newEvent.AddChild(ConvertToType(context.type()), Roles.Type);
      //      AddExplicitInterface(newEvent, ep.MemberName);
            var mn = context.member_name();
            newEvent.AddChild(Identifier.Create(mn.identifier().GetText(), Convert(mn.identifier().start)), Roles.Identifier);
           newEvent.AddChild(new CSharpTokenNode(Convert(context.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
           var evd = context.event_accessor_declarations();
           var addacc = evd.add_accessor_declaration();
            var raiseacc =evd.raise_accessor_declaration();
            var removeacc = evd.remove_accessor_declaration();
            if (addacc != null)
            {
                var addAccessor = new Accessor();
                AddAttributeSection(addAccessor, addacc.attributes());
                AddModifiers(addAccessor, addacc.accessor_modifier());
                addAccessor.AddChild(new CSharpTokenNode(Convert(addacc.ADD()), CustomEventDeclaration.AddKeywordRole), CustomEventDeclaration.AddKeywordRole);
           
                    var convBlock = addacc.block().Accept(this) as BlockStatement;
                    if (convBlock != null)
                        addAccessor.AddChild(convBlock, Roles.Body);
             
                newEvent.AddChild(addAccessor, CustomEventDeclaration.AddAccessorRole);
            }

            if (removeacc != null)
            {
                var removeAccessor = new Accessor();
                AddAttributeSection(removeAccessor, removeacc.attributes());
                AddModifiers(removeAccessor, removeacc.accessor_modifier());
                removeAccessor.AddChild(new CSharpTokenNode(Convert(removeacc.REMOVE()), CustomEventDeclaration.RemoveKeywordRole), CustomEventDeclaration.RemoveKeywordRole);

                var convBlock = removeacc.block().Accept(this) as BlockStatement;
                if (convBlock != null)
                    removeAccessor.AddChild(convBlock, Roles.Body);

                newEvent.AddChild(removeAccessor, CustomEventDeclaration.RemoveAccessorRole);
            }

            // TODO : Add raise accessor
          newEvent.AddChild(new CSharpTokenNode(Convert(context.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);
            

            typeStack.Peek().AddChild(newEvent, Roles.TypeMemberRole);

            return newEvent;
        }
        public override AstNode VisitField_declaration(RSharpParser.Field_declarationContext context)
        {
 
            var newField = new FieldDeclaration();
            AddAttributeSection(newField, context.attributes());
            AddModifiers(newField, context.member_modifiers());
            newField.AddChild(ConvertToType(context.type()), Roles.Type);
            var decls = context.variable_declarators();
            if (decls != null)
            {
                int i  =0;
                foreach (var decl in decls.variable_declarator())
                {
                 var  variable = new VariableInitializer();
                 var id = decl.identifier();
                 variable.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                 var init = decl.variable_initializer();
                 if (init != null)
                    {
                        variable.AddChild(new CSharpTokenNode(Convert(decl.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                        variable.AddChild((Expression)init.Accept(this), Roles.Expression);
                    }
                    newField.AddChild(variable, Roles.Variable);
                    var comma = decls.COMMA(i);
                    if (comma != null)
                        newField.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                    i++;
                }
            }

          newField.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

            typeStack.Peek().AddChild(newField, Roles.TypeMemberRole);
            return newField;
        }
        public override AstNode VisitConstant_declaration(RSharpParser.Constant_declarationContext context)
        {
            var newField = new FieldDeclaration();
            AddAttributeSection(newField, context.attributes());
            AddModifiers(newField, context.member_modifiers());
            newField.AddChild(new CSharpModifierToken(Convert(context.start), Modifiers.Const), EntityDeclaration.ModifierRole);
            newField.AddChild(ConvertToType(context.type()), Roles.Type);
            var decls = context.constant_declarators();
            if (decls != null)
            {
                int i = 0;
                foreach (var decl in decls.constant_declarator())
                {
                    var variable = new VariableInitializer();
                    var id = decl.identifier();
                    variable.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                    var init = decl.expression();
                    if (init != null)
                    {
                        variable.AddChild(new CSharpTokenNode(Convert(decl.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                        variable.AddChild((Expression)init.Accept(this), Roles.Expression);
                    }
                    newField.AddChild(variable, Roles.Variable);
                    var comma = decls.COMMA(i);
                    if (comma != null)
                        newField.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                    i++;
                }
            }

            newField.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);

            typeStack.Peek().AddChild(newField, Roles.TypeMemberRole);
            return newField;
        }
        // TODO:add other operators
        public override AstNode VisitOperator_declaration(RSharpParser.Operator_declarationContext context)
        {
            var newOperator = new OperatorDeclaration();
            newOperator.OperatorType = OperatorType.True;
            AddAttributeSection(newOperator, context.attributes());
            AddModifiers(newOperator, context.method_modifiers());
            newOperator.AddChild(ConvertToType(context.type()), Roles.Type);
            newOperator.AddChild(new CSharpTokenNode(Convert(context.OPERATOR()), OperatorDeclaration.OperatorKeywordRole), OperatorDeclaration.OperatorKeywordRole);
            var ovrloadable = context.overloadable_operator();
            var r = OperatorDeclaration.GetRole(newOperator.OperatorType);
            newOperator.AddChild(new CSharpTokenNode(Convert(ovrloadable.start), r), r);
            newOperator.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            foreach(var arg in context.arg_declaration())
                AddParameter(newOperator, arg);
            
           newOperator.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.RPar), Roles.RPar);

           var mbody = context.method_body().Accept(this);
           if (mbody != null)
           {
               var bodyBlock = mbody as BlockStatement;

               if (bodyBlock != null)
                   newOperator.AddChild(bodyBlock, Roles.Body);

           }

            typeStack.Peek().AddChild(newOperator, Roles.TypeMemberRole);
            return newOperator;
        }
        public override AstNode VisitConversion_operator_declarator(RSharpParser.Conversion_operator_declaratorContext context)
        {
            var newOperator = new OperatorDeclaration();
            newOperator.OperatorType = OperatorType.True;
            AddAttributeSection(newOperator, context.attributes());
            AddModifiers(newOperator, context.method_modifiers());
            var impl = context.IMPLICIT();
            var expl = context.EXPLICIT();
            if (impl != null)
                newOperator.AddChild(new CSharpTokenNode(Convert(impl), OperatorDeclaration.ImplicitRole), OperatorDeclaration.ImplicitRole);
            else if (expl != null)
                    newOperator.AddChild(new CSharpTokenNode(Convert(expl), OperatorDeclaration.ExplicitRole), OperatorDeclaration.ExplicitRole);
            newOperator.AddChild(new CSharpTokenNode(Convert(context.OPERATOR()), OperatorDeclaration.OperatorKeywordRole), OperatorDeclaration.OperatorKeywordRole);
            newOperator.AddChild(ConvertToType(context.type()), Roles.Type);
            newOperator.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            AddParameter(newOperator, context.arg_declaration());
            newOperator.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.RPar), Roles.RPar);

            var mbody = context.method_body().Accept(this);
            if (mbody != null)
            {
                var bodyBlock = mbody as BlockStatement;

                if (bodyBlock != null)
                    newOperator.AddChild(bodyBlock, Roles.Body);

            }

            typeStack.Peek().AddChild(newOperator, Roles.TypeMemberRole);
            return newOperator;
        }

        public override AstNode VisitEnum_member_declaration(RSharpParser.Enum_member_declarationContext context)
        {
            var newField = new EnumMemberDeclaration();
            AddAttributeSection(newField, context.attributes());
            var id = context.identifier();
            newField.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            var expr = context.expression();
            if (expr != null)
            {
                newField.AddChild(new CSharpTokenNode(Convert(context.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                newField.AddChild((Expression)expr.Accept(this), EnumMemberDeclaration.InitializerRole);
            }
     
            typeStack.Peek().AddChild(newField, Roles.TypeMemberRole);
            return newField;
        }
        public override AstNode VisitEnum_definition(RSharpParser.Enum_definitionContext context)
        {
            var newType = new TypeDeclaration();
            newType.ClassType = ClassType.Enum;
            AddAttributeSection(newType, context.attributes());
            AddModifiers(newType, context.type_modifiers());
            newType.AddChild(new CSharpTokenNode(Convert(context.ENUM()), Roles.EnumKeyword), Roles.EnumKeyword);
            var id = context.identifier();
            newType.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            var enumbase = context.enum_base();

            if (enumbase != null)
            {
              
                    newType.AddChild(new CSharpTokenNode(Convert(enumbase.COLON()), Roles.Colon), Roles.Colon);
                newType.AddChild(ConvertToType(enumbase.integral_type()), Roles.BaseType);
            }

          var enumbody = context.enum_body();
          newType.AddChild(new CSharpTokenNode(Convert(enumbody.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            typeStack.Push(newType);
            var mems = enumbody.enum_member_declaration();
            if (mems != null)
            {
                int i =0;
                foreach (var m in mems)
                {
                    Visit(m);
                    var comma = enumbody.COMMA(i);
                    if (comma != null) //last one is closing brace
                        newType.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        
                    i++;
                }
            }
            newType.AddChild(new CSharpTokenNode(Convert(enumbody.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);

            typeStack.Pop();
            AddType(newType);
            return newType;
        }
        public override AstNode VisitClass_definition(RSharpParser.Class_definitionContext context)
        {
            var newType = new TypeDeclaration();
            newType.ClassType = ClassType.Class;
            AddAttributeSection(newType, context.attributes());
            AddModifiers(newType, context.type_modifiers());
            newType.AddChild(new CSharpTokenNode(Convert(context.CLASS()), Roles.ClassKeyword), Roles.ClassKeyword);
            var id = context.identifier();
            newType.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            AddTypeParameters(newType, context.type_parameter_list());
            var basetypes = context.class_base();
            if (basetypes != null)
            {

                newType.AddChild(new CSharpTokenNode(Convert(basetypes.COLON()), Roles.Colon), Roles.Colon);
                int i = 0;
                foreach (var baseTypes in basetypes.class_type())
                {
                    newType.AddChild(ConvertToType(baseTypes), Roles.BaseType);
                    var comma = basetypes.COMMA(i);
                    if (comma != null)
                    {
                        newType.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        i++;
                    }
                }
            }

            AddConstraints(newType, context.type_parameter_constraints_clauses());
            var clbody = context.class_body();
                newType.AddChild(new CSharpTokenNode(Convert(clbody.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            typeStack.Push(newType);
            base.Visit(clbody.class_member_declarations());
                newType.AddChild(new CSharpTokenNode(Convert(clbody.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);

            typeStack.Pop();
            AddType(newType);
            return newType;
        }
        public override AstNode VisitClass_member_declaration(RSharpParser.Class_member_declarationContext context)
        {
            return base.VisitClass_member_declaration(context);
        }
        public override AstNode VisitClass_member_declarations(RSharpParser.Class_member_declarationsContext context)
        {
            return base.VisitClass_member_declarations(context);
        }
        public override AstNode VisitCommon_member_declaration(RSharpParser.Common_member_declarationContext context)
        {
            return base.VisitCommon_member_declaration(context);
        }
        public override AstNode VisitDefault_member_declaration(RSharpParser.Default_member_declarationContext context)
        {
            return base.VisitDefault_member_declaration(context);
        }
        public override AstNode VisitStruct_definition(RSharpParser.Struct_definitionContext context)
        {
            var newType = new TypeDeclaration();
            newType.ClassType = ClassType.Struct;
            AddAttributeSection(newType, context.attributes());
            AddModifiers(newType, context.type_modifiers());
                newType.AddChild(new CSharpTokenNode(Convert(context.STRUCT()), Roles.StructKeyword), Roles.StructKeyword);
                var id = context.identifier();
                newType.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                AddTypeParameters(newType, context.type_parameter_list());

                var structbase = context.struct_interfaces();
                if (structbase != null)
            {

                newType.AddChild(new CSharpTokenNode(Convert(structbase.COLON()), Roles.Colon), Roles.Colon);
           
                int i = 0;
                var il = structbase.interface_type_list();
                foreach (var baseTypes in il.package_or_type_name())
                {
                    newType.AddChild(ConvertToType(baseTypes), Roles.BaseType);
                    var comma = il.COMMA(i);
                    if (comma != null)
                    {
                        newType.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        i++;
                    }
                }
            }

            AddConstraints(newType, context.type_parameter_constraints_clauses());
            var clbody = context.struct_body();
            newType.AddChild(new CSharpTokenNode(Convert(clbody.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            typeStack.Push(newType);
            base.Visit(clbody);
            newType.AddChild(new CSharpTokenNode(Convert(clbody.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);

            typeStack.Pop();
            AddType(newType);
            return newType;
        }
        public override AstNode VisitStruct_body(RSharpParser.Struct_bodyContext context)
        {
            return base.VisitStruct_body(context);
        }
        public override AstNode VisitInterface_definition(RSharpParser.Interface_definitionContext context)
        {
            var newType = new TypeDeclaration();
            newType.ClassType = ClassType.Interface;
            AddAttributeSection(newType, context.attributes());
            AddModifiers(newType, context.type_modifiers());
            newType.AddChild(new CSharpTokenNode(Convert(context.INTERFACE()), Roles.InterfaceKeyword), Roles.InterfaceKeyword);
            var id = context.identifier();
            newType.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            AddTypeParameters(newType, context.type_parameter_list());


            var ibase = context.interface_base();
            if (ibase != null)
            {

                newType.AddChild(new CSharpTokenNode(Convert(ibase.COLON()), Roles.Colon), Roles.Colon);

                int i = 0;
                var il = ibase.interface_type_list();
                foreach (var baseTypes in il.package_or_type_name())
                {
                    newType.AddChild(ConvertToType(baseTypes), Roles.BaseType);
                    var comma = il.COMMA(i);
                    if (comma != null)
                    {
                        newType.AddChild(new CSharpTokenNode(Convert(comma), Roles.Comma), Roles.Comma);
                        i++;
                    }
                }
            }

            AddConstraints(newType, context.type_parameter_constraints_clauses());
            var clbody = context.interface_body();
            newType.AddChild(new CSharpTokenNode(Convert(clbody.OPEN_BRACE()), Roles.LBrace), Roles.LBrace);
            typeStack.Push(newType);
            base.Visit(clbody);
            newType.AddChild(new CSharpTokenNode(Convert(clbody.CLOSE_BRACE()), Roles.RBrace), Roles.RBrace);

            typeStack.Pop();
            AddType(newType);
            return newType;
        }
        public override AstNode VisitInterface_body(RSharpParser.Interface_bodyContext context)
        {
            return base.VisitInterface_body(context);
        }
        // TODO:interface members
        public override AstNode VisitDelegate_definition(RSharpParser.Delegate_definitionContext context)
        {
            var newDelegate = new DelegateDeclaration();
            AddAttributeSection(newDelegate, context.attributes());
            AddModifiers(newDelegate, context.type_modifiers());
           newDelegate.AddChild(new CSharpTokenNode(Convert(context.DELEGATE()), Roles.DelegateKeyword), Roles.DelegateKeyword);
           newDelegate.AddChild(ConvertToType(context.return_type()), Roles.Type);
            var id = context.identifier();
            newDelegate.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
            AddTypeParameters(newDelegate, context.type_parameter_list());
            newDelegate.AddChild(new CSharpTokenNode(Convert(context.OPEN_PARENS()), Roles.LPar), Roles.LPar);
            AddParameters(newDelegate, context.formal_parameter_list());
            newDelegate.AddChild(new CSharpTokenNode(Convert(context.CLOSE_PARENS()), Roles.RPar), Roles.RPar);
            AddConstraints(newDelegate, context.type_parameter_constraints_clauses());
            newDelegate.AddChild(new CSharpTokenNode(Convert(context.SEMICOLON()), Roles.Semicolon), Roles.Semicolon);
            AddType(newDelegate);
            return newDelegate;
        }



        #region LINQ
        // query
        QueryExpression currentQuery = null;
        public override AstNode VisitQuery_expression(RSharpParser.Query_expressionContext context)
        {
                currentQuery = new QueryExpression();
                var frm = context.from_clause().Accept(this);
               // add first from
                currentQuery.AddChild((QueryClause)frm, QueryExpression.ClauseRole);
                context.query_body().Accept(this);
                return currentQuery;
        
        }
        public override AstNode VisitLet_clause(RSharpParser.Let_clauseContext context)
        {
            var result = new QueryLetClause();
            result.AddChild(new CSharpTokenNode(Convert(context.LET()), QueryLetClause.LetKeywordRole), QueryLetClause.LetKeywordRole);
            var id = context.identifier();
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
                result.AddChild(new CSharpTokenNode(Convert(context.ASSIGNMENT()), Roles.Assign), Roles.Assign);
                result.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            return result;
        }
        public override AstNode VisitCombined_join_clause(RSharpParser.Combined_join_clauseContext context)
        {
            var result = new QueryJoinClause();
            result.AddChild(new CSharpTokenNode(Convert(context.JOIN()), QueryJoinClause.JoinKeywordRole), QueryJoinClause.JoinKeywordRole);
            var tp = context.type();
            if(tp != null)
                result.AddChild(ConvertToType(tp), QueryJoinClause.TypeRole);
            var id = context.identifier(0);
            result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), QueryJoinClause.JoinIdentifierRole);
            result.AddChild(new CSharpTokenNode(Convert(context.IN()), QueryJoinClause.InKeywordRole), QueryJoinClause.InKeywordRole);
            result.AddChild((Expression)context.expression(0).Accept(this), QueryJoinClause.InExpressionRole);
            result.AddChild(new CSharpTokenNode(Convert(context.ON()), QueryJoinClause.OnKeywordRole), QueryJoinClause.OnKeywordRole);
            result.AddChild((Expression)context.expression(1).Accept(this), QueryJoinClause.OnExpressionRole);
            result.AddChild(new CSharpTokenNode(Convert(context.EQUALS()), QueryJoinClause.EqualsKeywordRole), QueryJoinClause.EqualsKeywordRole);
            result.AddChild((Expression)context.expression(2).Accept(this), QueryJoinClause.EqualsExpressionRole);
            var into = context.INTO();
            if (into != null)
            {
                id = context.identifier(1);
                result.AddChild(new CSharpTokenNode(Convert(into), QueryJoinClause.IntoKeywordRole), QueryJoinClause.IntoKeywordRole);
                result.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), QueryJoinClause.IntoIdentifierRole);
            }
            return result;
        }
        public override AstNode VisitWhere_clause(RSharpParser.Where_clauseContext context)
        {
            var result = new QueryWhereClause();
            result.AddChild(new CSharpTokenNode(Convert(context.WHERE()), QueryWhereClause.WhereKeywordRole), QueryWhereClause.WhereKeywordRole);
            result.AddChild((Expression)context.WHERE().Accept(this), Roles.Condition);
            return result;
        }
        public override AstNode VisitSelect_or_group_clause(RSharpParser.Select_or_group_clauseContext context)
        {
            var sel = context.select_clause();
            if (sel != null)
                return sel.Accept(this);
            else return context.group_clause().Accept(this);
        }
        public override AstNode VisitSelect_clause(RSharpParser.Select_clauseContext context)
        {
            var result = new QuerySelectClause();
            result.AddChild(new CSharpTokenNode(Convert(context.SELECT()), QuerySelectClause.SelectKeywordRole), QuerySelectClause.SelectKeywordRole);
            result.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            return result;
        }
        public override AstNode VisitGroup_clause(RSharpParser.Group_clauseContext context)
        {
            var result = new QueryGroupClause();
            result.AddChild(new CSharpTokenNode(Convert(context.GROUP()), QueryGroupClause.GroupKeywordRole), QueryGroupClause.GroupKeywordRole);
            result.AddChild((Expression)context.expression(0).Accept(this), QueryGroupClause.ProjectionRole);
            result.AddChild(new CSharpTokenNode(Convert(context.BY()), QueryGroupClause.ByKeywordRole), QueryGroupClause.ByKeywordRole);
            result.AddChild((Expression)context.expression(1).Accept(this), QueryGroupClause.KeyRole);
            return result;
        }
        public override AstNode VisitOrderby_clause(RSharpParser.Orderby_clauseContext context)
        {
            QueryOrderClause result = new QueryOrderClause();
            result.AddChild(new CSharpTokenNode(Convert(context.ORDERBY()), QueryOrderClause.OrderbyKeywordRole), QueryOrderClause.OrderbyKeywordRole);
          
            foreach (var ordering in context.ordering())
                result.AddChild((QueryOrdering)ordering.Accept(this), QueryOrderClause.OrderingRole);

            return result;
        }
        public override AstNode VisitOrdering(RSharpParser.OrderingContext context)
        {
            var ordering = new QueryOrdering();
            ordering.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            var asc = context.ASCENDING();
            var desc = context.DESCENDING();
            if (asc != null)
            {
                ordering.Direction = QueryOrderingDirection.Ascending;
                ordering.AddChild(new CSharpTokenNode(Convert(asc), QueryOrdering.AscendingKeywordRole), QueryOrdering.AscendingKeywordRole);
            }
            else
            {
                ordering.Direction = QueryOrderingDirection.Descending;
                ordering.AddChild(new CSharpTokenNode(Convert(desc), QueryOrdering.DescendingKeywordRole), QueryOrdering.DescendingKeywordRole);
            }

            return ordering;
        }
        public override AstNode VisitFrom_clause(RSharpParser.From_clauseContext context)
        {
            var fromClause = new QueryFromClause();
            fromClause.AddChild(new CSharpTokenNode(Convert(context.FROM()), QueryFromClause.FromKeywordRole), QueryFromClause.FromKeywordRole);
            var tp = context.type();
            var id = context.identifier();
            if (tp != null)
                fromClause.AddChild(ConvertToType(tp), Roles.Type);
            fromClause.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
          fromClause.AddChild(new CSharpTokenNode(Convert(context.IN()), QueryFromClause.InKeywordRole), QueryFromClause.InKeywordRole);
           fromClause.AddChild((Expression)context.expression().Accept(this), Roles.Expression);
            return fromClause;
        }
        public override AstNode VisitQuery_continuation(RSharpParser.Query_continuationContext context)
        {
          
            var intoClause = new QueryContinuationClause();
            intoClause.AddChild(new CSharpTokenNode(Convert(context.INTO()), QueryContinuationClause.IntoKeywordRole), QueryContinuationClause.IntoKeywordRole);
            var id = context.identifier();
            intoClause.AddChild(Identifier.Create(id.GetText(), Convert(id.start)), Roles.Identifier);
           
            return intoClause;
        }
        public override AstNode VisitQuery_body(RSharpParser.Query_bodyContext context)
        {
            // query clauses
                foreach(var b in context.query_body_clause())
                    currentQuery.AddChild((QueryClause)b.Accept(this), QueryExpression.ClauseRole);

            // select or group clause
                currentQuery.AddChild((QueryClause)context.select_or_group_clause().Accept(this), QueryExpression.ClauseRole);
            // query continuation
                var qc = context.query_continuation();
                if (qc != null)
                {
                    QueryExpression precedingquery = currentQuery;
                    currentQuery = new QueryExpression();
                    QueryContinuationClause q = (QueryContinuationClause)qc.Accept(this);
                    q.InsertChildAfter(null, precedingquery, QueryContinuationClause.PrecedingQueryRole);
                    currentQuery.AddChild(q, QueryExpression.ClauseRole);
                    qc.query_body().Accept(this);
                }
                return null;

        }
        public override AstNode VisitQuery_body_clause(RSharpParser.Query_body_clauseContext context)
        {
            ParserRuleContext child = context.from_clause();
            if (child != null)
                return child.Accept(this);

            child = context.let_clause();
            if (child != null)
                return child.Accept(this);

            child = context.orderby_clause();
            if (child != null)
                return child.Accept(this);

            child = context.where_clause();
            if (child != null)
                return child.Accept(this);

            return context.combined_join_clause().Accept(this);
        }
        #endregion


        public override AstNode VisitAccessor_modifier(RSharpParser.Accessor_modifierContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttribute(RSharpParser.AttributeContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitArgument(RSharpParser.ArgumentContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitArgument_list(RSharpParser.Argument_listContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttribute_list(RSharpParser.Attribute_listContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttribute_argument(RSharpParser.Attribute_argumentContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitEvent_accessor_declarations(RSharpParser.Event_accessor_declarationsContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAdd_accessor_declaration(RSharpParser.Add_accessor_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitRaise_accessor_declaration(RSharpParser.Raise_accessor_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitRemove_accessor_declaration(RSharpParser.Remove_accessor_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitFormal_parameter_list(RSharpParser.Formal_parameter_listContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitFixed_parameters(RSharpParser.Fixed_parametersContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitFixed_parameter(RSharpParser.Fixed_parameterContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitParameter_array(RSharpParser.Parameter_arrayContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitParameter_modifier(RSharpParser.Parameter_modifierContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitType_argument_list(RSharpParser.Type_argument_listContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitType_parameter(RSharpParser.Type_parameterContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitType_parameter_constraints(RSharpParser.Type_parameter_constraintsContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitType_parameter_constraints_clause(RSharpParser.Type_parameter_constraints_clauseContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitType_parameter_list(RSharpParser.Type_parameter_listContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitClass_type(RSharpParser.Class_typeContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitPrimary_constraint(RSharpParser.Primary_constraintContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitSecondary_constraints(RSharpParser.Secondary_constraintsContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitIndexer_argument(RSharpParser.Indexer_argumentContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitBracket_expression(RSharpParser.Bracket_expressionContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttributes(RSharpParser.AttributesContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttribute_section(RSharpParser.Attribute_sectionContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitAttribute_target(RSharpParser.Attribute_targetContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitArg_declaration(RSharpParser.Arg_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitGlobal_attribute_target(RSharpParser.Global_attribute_targetContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitGlobal_attribute_section(RSharpParser.Global_attribute_sectionContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitGet_accessor_declaration(RSharpParser.Get_accessor_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitSet_accessor_declaration(RSharpParser.Set_accessor_declarationContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitVariable_declarator(RSharpParser.Variable_declaratorContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitVariable_declarators(RSharpParser.Variable_declaratorsContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitConstant_declarator(RSharpParser.Constant_declaratorContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitConstant_declarators(RSharpParser.Constant_declaratorsContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitEnum_body(RSharpParser.Enum_bodyContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitEnum_base(RSharpParser.Enum_baseContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitClass_base(RSharpParser.Class_baseContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitClass_body(RSharpParser.Class_bodyContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitStruct_interfaces(RSharpParser.Struct_interfacesContext context)
        {
            throw new HandledByParentException();
        }
        public override AstNode VisitInterface_base(RSharpParser.Interface_baseContext context)
        {
            throw new HandledByParentException();
        }
     
    }


    public class HandledByParentException : NotSupportedException
    {

    }
}
