// 
// CSharpParser.cs
//
// Author:
//       Mike Krüger <mkrueger@novell.com>
// 
// Copyright (c) 2009 Novell, Inc (http://www.novell.com)
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
using System;
using System.Linq;
using System.Collections.Generic;
using System.IO;
using ICSharpCode.NRefactory.Editor;
using ICSharpCode.NRefactory.MonoRava;
using ICSharpCode.NRefactory.TypeSystem;
using Antlr4.Runtime;
using ICSharpCode.NRefactory.Rava.Parser;

namespace ICSharpCode.NRefactory.Rava
{
	public class CSharpParser
	{
		CompilerSettings compilerSettings;


    
		public CSharpParser()
        {
 
			compilerSettings = new CompilerSettings();
		}

		public CSharpParser(CompilerSettings args)
		{
			compilerSettings = args ?? new CompilerSettings();
		}

        //void InsertComments(CompilerCompilationUnit top, ConversionVisitor conversionVisitor)
        //{
        //    AstNode insertionPoint = conversionVisitor.Unit.FirstChild;
        //    foreach (var special in top.SpecialsBag.Specials) {
        //        AstNode newLeaf = null;
        //        Role role = null;
        //        bool isDocumentationComment = false;
        //        var comment = special as SpecialsBag.Comment;
        //        if (comment != null) {
        //            // HACK: multiline documentation comment detection; better move this logic into the mcs tokenizer
        //            bool isMultilineDocumentationComment = (comment.CommentType == SpecialsBag.CommentType.Multi && comment.Content.StartsWith("*", StringComparison.Ordinal) && !comment.Content.StartsWith("**", StringComparison.Ordinal));
        //            isDocumentationComment = comment.CommentType == SpecialsBag.CommentType.Documentation || isMultilineDocumentationComment;
        //            if (conversionVisitor.convertTypeSystemMode && !isDocumentationComment)
        //                continue;
        //            var type = isMultilineDocumentationComment ? CommentType.MultiLineDocumentation : (CommentType)comment.CommentType;
        //            var start = new TextLocation(comment.Line, comment.Col);
        //            var end = new TextLocation(comment.EndLine, comment.EndCol);
        //            newLeaf = new Comment(type, start, end) {
        //                StartsLine = comment.StartsLine,
        //                Content = isMultilineDocumentationComment ? comment.Content.Substring(1) : comment.Content
        //            };
        //            role = Roles.Comment;
        //        } else if (!GenerateTypeSystemMode) {
        //            var pragmaDirective = special as SpecialsBag.PragmaPreProcessorDirective;
        //            if (pragmaDirective != null) {
        //                var pragma = new PragmaWarningPreprocessorDirective(new TextLocation(pragmaDirective.Line, pragmaDirective.Col), new TextLocation(pragmaDirective.EndLine, pragmaDirective.EndCol));
        //                pragma.AddChild(new CSharpTokenNode(new TextLocation(pragmaDirective.Line, pragmaDirective.Col), PragmaWarningPreprocessorDirective.PragmaKeywordRole), PragmaWarningPreprocessorDirective.PragmaKeywordRole);
        //                pragma.AddChild(new CSharpTokenNode(new TextLocation(pragmaDirective.Line, pragmaDirective.WarningColumn), PragmaWarningPreprocessorDirective.WarningKeywordRole), PragmaWarningPreprocessorDirective.WarningKeywordRole);
        //                var pragmaRole = pragmaDirective.Disalbe ? PragmaWarningPreprocessorDirective.DisableKeywordRole : PragmaWarningPreprocessorDirective.RestoreKeywordRole;
        //                pragma.AddChild(new CSharpTokenNode(new TextLocation(pragmaDirective.Line, pragmaDirective.DisableRestoreColumn), pragmaRole), pragmaRole);
        //                foreach (var code in pragmaDirective.Codes) {
        //                    pragma.AddChild((PrimitiveExpression)conversionVisitor.Visit(code), PragmaWarningPreprocessorDirective.WarningRole);
        //                }
        //                newLeaf = pragma;
        //                role = Roles.PreProcessorDirective;
        //                goto end;
        //            }
        //            var lineDirective = special as SpecialsBag.LineProcessorDirective;
        //            if (lineDirective != null) {
        //                var pragma = new LinePreprocessorDirective(new TextLocation(lineDirective.Line, lineDirective.Col), new TextLocation(lineDirective.EndLine, lineDirective.EndCol));
        //                pragma.LineNumber = lineDirective.LineNumber;
        //                pragma.FileName = lineDirective.FileName;
        //                newLeaf = pragma;
        //                role = Roles.PreProcessorDirective;
        //                goto end;
        //            }
        //            var directive = special as SpecialsBag.PreProcessorDirective;
        //            if (directive != null) {
        //                newLeaf = new PreProcessorDirective((PreProcessorDirectiveType)((int)directive.Cmd & 0xF), new TextLocation(directive.Line, directive.Col), new TextLocation(directive.EndLine, directive.EndCol)) {
        //                    Argument = directive.Arg,
        //                    Take = directive.Take
        //                };
        //                role = Roles.PreProcessorDirective;
        //            }
        //            end:
        //            ;
        //        }
        //        if (newLeaf != null) {
        //            InsertComment(ref insertionPoint, newLeaf, role, isDocumentationComment, conversionVisitor.Unit);
        //        }
        //    }
        //    if (!GenerateTypeSystemMode) {
        //        // We cannot insert newlines in the same loop as comments/preprocessor directives
        //        // because they are not correctly ordered in the specials bag
        //        insertionPoint = conversionVisitor.Unit.FirstChild;
        //        for (int i = 0; i < top.SpecialsBag.Specials.Count; i++) {
        //            var newLine = top.SpecialsBag.Specials [i] as SpecialsBag.NewLineToken;
        //            if (newLine != null) {
        //                var newLeaf = new NewLineNode(new TextLocation(newLine.Line, newLine.Col + 1));
        //                newLeaf.NewLineType = newLine.NewLine == SpecialsBag.NewLine.Unix ? UnicodeNewline.LF : UnicodeNewline.CRLF;
        //                InsertComment(ref insertionPoint, newLeaf, Roles.NewLine, false, conversionVisitor.Unit);
        //            }
        //        }
        //    }
        //}

		static void InsertComment(ref AstNode insertionPoint, AstNode newNode, Role role, bool isDocumentationComment, AstNode rootNode)
		{
			TextLocation insertAt = newNode.StartLocation;
			// Advance insertionPoint to the first node that has a start location >= insertAt
			while (insertionPoint != null && insertionPoint.StartLocation < insertAt) {
				// Enter the current node if insertAt is within
				while (insertAt < insertionPoint.EndLocation && insertionPoint.FirstChild != null) {
					insertionPoint = insertionPoint.FirstChild;
				}
				// Go to next node (insertionPoint.NextSibling if it exists; otherwise the next sibling of the parent node etc.)
				insertionPoint = insertionPoint.GetNextNode();
			}
			// As a special case, XmlDoc gets inserted at the beginning of the entity declaration
			if (isDocumentationComment && insertionPoint is EntityDeclaration && insertionPoint.FirstChild != null) {
				insertionPoint = insertionPoint.FirstChild;
			}
			if (insertionPoint == null) {
				// we're at the end of the compilation unit
				rootNode.AddChildUnsafe(newNode, role);
			} else {
				insertionPoint.Parent.InsertChildBeforeUnsafe(insertionPoint, newNode, role);
			}
		}

        public class ErrorReportPrinter : ConsoleErrorListener<IToken >
		{
			readonly string fileName;
			public readonly List<Error> Errors = new List<Error>();
            public int ErrorsCount { get { return Errors.Count; } }
            public int WarningsCount = 0;
			public ErrorReportPrinter(string fileName)
			{
				this.fileName = fileName;
			}

            //public override void Print(AbstractMessage msg, bool showFullPath)
            //{
            //    base.Print(msg, showFullPath);
            //    var newError = new Error(msg.IsWarning ? ErrorType.Warning : ErrorType.Error, msg.Text, new DomRegion(fileName, msg.Location.Row, msg.Location.Column));
            //    Errors.Add(newError);
            //}
		}

		ErrorReportPrinter errorReportPrinter = new ErrorReportPrinter(null);

		[Obsolete("Use the Errors/Warnings/ErrorsAndWarnings properties instead")]
		public ErrorReportPrinter ErrorPrinter {
			get {
				return errorReportPrinter;
			}
		}

		public bool HasErrors {
			get {
				return errorReportPrinter.ErrorsCount > 0;
			}
		}

		public bool HasWarnings {
			get {
				return errorReportPrinter.WarningsCount > 0;
			}
		}

		public IEnumerable<Error> Errors {
			get {
				return errorReportPrinter.Errors.Where(e => e.ErrorType == ErrorType.Error);
			}
		}

		public IEnumerable<Error> Warnings {
			get {
				return errorReportPrinter.Errors.Where(e => e.ErrorType == ErrorType.Warning);
			}
		}

		public IEnumerable<Error> ErrorsAndWarnings {
			get { return errorReportPrinter.Errors; }
		}

		/// <summary>
		/// Parses a C# code file.
		/// </summary>
		/// <param name="package">The source code to parse.</param>
		/// <param name="fileName">The file name. Used to identify the file (e.g. when building a type system).
		/// This can be an arbitrary identifier, NRefactory never tries to access the file on disk.</param>
		/// <returns>Returns the syntax tree.</returns>
		public SyntaxTree Parse(string program, string fileName = "")
		{
			return Parse(new StringTextSource(program), fileName);
		}

		/// <summary>
		/// Parses a C# code file.
		/// </summary>
		/// <param name="reader">The text reader containing the source code to parse.</param>
		/// <param name="fileName">The file name. Used to identify the file (e.g. when building a type system).
		/// This can be an arbitrary identifier, NRefactory never tries to access the file on disk.</param>
		/// <returns>Returns the syntax tree.</returns>
		public SyntaxTree Parse(TextReader reader, string fileName = "")
		{
			return Parse(new StringTextSource(reader.ReadToEnd()), fileName);
		}

		/// <summary>
		/// Converts a ICSharpCode.NRefactory.MonoRava syntax tree into an NRefactory syntax tree.
		/// </summary>
		public SyntaxTree Parse(RSharpParser.Compilation_unitContext top, string fileName)
		{
			if (top == null) {
				return null;
			}
			ConversionVisitor conversionVisitor = new ConversionVisitor();
			top.Accept(conversionVisitor);
            ////InsertComments(top, conversionVisitor);
            //if (CompilationUnitCallback != null) {
            //    CompilationUnitCallback(top);
            //}
            //var expr = top.LastYYValue as ICSharpCode.NRefactory.MonoRava.Expression;
            //if (expr != null)
            //    conversionVisitor.Unit.TopExpression = expr.Accept(conversionVisitor) as AstNode;

            //conversionVisitor.Unit.FileName = fileName;
            //var conditionals = new List<string>();
            //foreach (var settings in compilerSettings.ConditionalSymbols) {
            //    if (top.Conditionals.ContainsKey(settings) && !top.Conditionals [settings])
            //        continue;
            //    conditionals.Add(settings);
            //}
            //foreach (var kv in top.Conditionals) {
            //    if (!kv.Value || compilerSettings.ConditionalSymbols.Contains(kv.Key))
            //        continue;
            //    conditionals.Add(kv.Key);
            //}
            //conversionVisitor.Unit.ConditionalSymbols = conditionals;
			return conversionVisitor.Unit;
		}

		public CompilerSettings CompilerSettings {
			get { return compilerSettings; }
			set {
				if (value == null)
					throw new ArgumentNullException();
				compilerSettings = value;
			}
		}

		/// <summary>
		/// Callback that gets called with the ICSharpCode.NRefactory.MonoRava syntax tree whenever some code is parsed.
		/// </summary>
		public Action<RSharpParser.Compilation_unitContext> CompilationUnitCallback {
			get;
			set;
		}

		/// <summary>
		/// Specifies whether to run the parser in a special mode for generating the type system.
		/// If this property is true, the syntax tree will only contain nodes relevant for the
		/// <see cref="SyntaxTree.ToTypeSystem()"/> call and might be missing other nodes (e.g. method bodies).
		/// The default is false.
		/// </summary>
		public bool GenerateTypeSystemMode {
			get;
			set;
		}

		TextLocation initialLocation = new TextLocation(1, 1);

		/// <summary>
		/// Specifies the text location where parsing starts.
		/// This property can be used when parsing a part of a file to make the locations of the AstNodes
		/// refer to the position in the whole file.
		/// The default is (1,1).
		/// </summary>
		public TextLocation InitialLocation {
			get { return initialLocation; }
			set { initialLocation = value; }
		}

		internal static object parseLock = new object();

		/// <summary>
		/// Parses a C# code file.
		/// </summary>
		/// <param name="stream">The stream containing the source code to parse.</param>
		/// <param name="fileName">The file name. Used to identify the file (e.g. when building a type system).
		/// This can be an arbitrary identifier, NRefactory never tries to access the file on disk.</param>
		/// <returns>Returns the syntax tree.</returns>
		public SyntaxTree Parse(Stream stream, string fileName = "")
		{
			return Parse(new StreamReader(stream), fileName);
		}

		/// <summary>
		/// Parses a C# code file.
		/// </summary>
		/// <param name="package">The source code to parse.</param>
		/// <param name="fileName">The file name. Used to identify the file (e.g. when building a type system).
		/// This can be an arbitrary identifier, NRefactory never tries to access the file on disk.</param>
		/// <returns>Returns the syntax tree.</returns>
		public SyntaxTree Parse(ITextSource program, string fileName = "")
		{
			return Parse(program, fileName, initialLocation.Line, initialLocation.Column);
		}
      public  RSharpParser.Compilation_unitContext Parse(ITextSource rs, IAntlrErrorListener<IToken> errorlist)
        {
               List<IToken> codeTokens = new List<IToken>();
            List<IToken> commentTokens = new List<IToken>();
                string code = rs.Text;
                Lexer preprocessorLexer = new RSharpLexer(new AntlrInputStream(code));
                //// Collect all tokens with lexer (CSharpLexer.g4).
                //var tokens = preprocessorLexer.GetAllTokens();
                //var directiveTokens = new List<IToken>();
                //var directiveTokenSource = new ListTokenSource(directiveTokens);
                //var directiveTokenStream = new CommonTokenStream(directiveTokenSource, RSharpLexer.DIRECTIVE);
                //RSharpPreprocessorParser preprocessorParser = new RSharpPreprocessorParser(directiveTokenStream);

                //int index = 0;
                //bool compiliedTokens = true;
                //while (index < tokens.Count)
                //{
                //    var token = tokens[index];
                //    if (token.Type == RSharpLexer.SHARP)
                //    {
                //        directiveTokens.Clear();
                //        int directiveTokenIndex = index + 1;
                //        // Collect all preprocessor directive tokens.
                //        while (directiveTokenIndex < tokens.Count &&
                //               tokens[directiveTokenIndex].Type != RSharpLexer.Eof &&
                //               tokens[directiveTokenIndex].Type != RSharpLexer.DIRECTIVE_NEW_LINE &&
                //               tokens[directiveTokenIndex].Type != RSharpLexer.SHARP)
                //        {
                //            IToken t = tokens[directiveTokenIndex];
                //            if (t.Channel == RSharpLexer.COMMENTS_CHANNEL)
                //            {
                //                commentTokens.Add(t);
                //            }
                //            else if (t.Channel != Lexer.Hidden)
                //            {
                //                directiveTokens.Add(tokens[directiveTokenIndex]);
                //            }
                //            directiveTokenIndex++;
                //        }

                //        directiveTokenSource = new ListTokenSource(directiveTokens);
                //        directiveTokenStream = new CommonTokenStream(directiveTokenSource, RSharpLexer.DIRECTIVE);
                //        preprocessorParser.SetInputStream(directiveTokenStream);
                //        preprocessorParser.Reset();
                //        // Parse condition in preprocessor directive (based on CSharpPreprocessorParser.g4 grammar).
                //        RSharpPreprocessorParser.Preprocessor_directiveContext directive = preprocessorParser.preprocessor_directive();
                //        // if true than next code is valid and not ignored.
                //        compiliedTokens = directive.value;
                //        index = directiveTokenIndex - 1;
                //    }
                //    else if (token.Channel == RSharpLexer.COMMENTS_CHANNEL)
                //    {
                //        commentTokens.Add(token); // Colect comment tokens (if required).
                //    }
                //    else if (token.Channel != Lexer.Hidden && token.Type != RSharpLexer.DIRECTIVE_NEW_LINE && compiliedTokens)
                //    {
                //        codeTokens.Add(token); // Collect code tokens.
                //    }
                //    index++;
                //}

                //// At second stage tokens parsed in usual way.
                //var codeTokenSource = new ListTokenSource(codeTokens);
                var codeTokenStream = new CommonTokenStream(preprocessorLexer);
               RSharpParser parser = new RSharpParser(codeTokenStream);
                parser.RemoveErrorListeners();
                parser.AddErrorListener(errorlist);
           
                // Parse syntax tree (CSharpParser.g4)
                return parser.compilation_unit();

        }
		SyntaxTree Parse(ITextSource program, string fileName, int initialLine, int initialColumn)
		{
			lock (parseLock) {
               errorReportPrinter = new ErrorReportPrinter("");
               var parser = Parse(program, errorReportPrinter);
               var unit = Parse(parser, fileName);
                unit.FileName = fileName;
				return unit;
			}
		}

		public IEnumerable<EntityDeclaration> ParseTypeMembers(string code)
		{
			return ParseTypeMembers(code, initialLocation.Line, initialLocation.Column);
		}

		IEnumerable<EntityDeclaration> ParseTypeMembers(string code, int initialLine, int initialColumn)
		{
			const string prefix = "unsafe partial class MyClass { ";
			var syntaxTree = Parse(new StringTextSource(prefix + code + "}"), "parsed.rv", initialLine, initialColumn - prefix.Length);
			if (syntaxTree == null)
				return Enumerable.Empty<EntityDeclaration>();
			var td = syntaxTree.FirstChild as TypeDeclaration;

			if (td != null) {
				var members = td.Members.ToArray();
				// detach members from parent
				foreach (var m in members)
					m.Remove();
				return members;
			}
			return Enumerable.Empty<EntityDeclaration>();
		}

		public IEnumerable<Statement> ParseStatements(string code)
		{
			return ParseStatements(code, initialLocation.Line, initialLocation.Column);
		}

		IEnumerable<Statement> ParseStatements(string code, int initialLine, int initialColumn)
		{
			// the dummy method is async so that 'await' expressions are parsed as expected
			const string prefix = "async void M() { ";
			var members = ParseTypeMembers(prefix + code + "}", initialLine, initialColumn - prefix.Length);
			var method = members.FirstOrDefault() as MethodDeclaration;
			if (method != null && method.Body != null) {
				var statements = method.Body.Statements.ToArray();
				// detach statements from parent
				foreach (var st in statements)
					st.Remove();
				return statements;
			}
			return Enumerable.Empty<Statement>();
		}

		public AstType ParseTypeReference(string code)
		{
			var members = ParseTypeMembers(code + " a;");
			var field = members.FirstOrDefault() as FieldDeclaration;
			if (field != null) {
				AstType type = field.ReturnType;
				type.Remove();
				return type;
			}
			return AstType.Null;
		}

		public Expression ParseExpression(string code)
		{
			const string prefix = "tmp = ";
			var statements = ParseStatements(prefix + code + ";", initialLocation.Line, initialLocation.Column - prefix.Length);
			var es = statements.FirstOrDefault() as ExpressionStatement;
			if (es != null) {
				var ae = es.Expression as AssignmentExpression;
				if (ae != null) {
					Expression expr = ae.Right;
					expr.Remove();
					return expr;
				}
			}
			return Expression.Null;
		}
		/*
		/// <summary>
		/// Parses a file snippet; guessing what the code snippet represents (whole file, type members, block, type reference, expression).
		/// </summary>
		public AstNode ParseSnippet (string code)
		{
			// TODO: add support for parsing a part of a file
			throw new NotImplementedException ();
		}
		 */
		public DocumentationReference ParseDocumentationReference(string cref)
		{
//            // see ICSharpCode.NRefactory.MonoRava.DocumentationBuilder.HandleXrefCommon
//            if (cref == null)
//                throw new ArgumentNullException("cref");
			
//            // Additional symbols for < and > are allowed for easier XML typing
//            cref = cref.Replace('{', '<').Replace('}', '>');
			
//            lock (parseLock) {
//                errorReportPrinter = new ErrorReportPrinter("");
//                var ctx = new CompilerContext(compilerSettings.ToMono(), errorReportPrinter);
//                ctx.Settings.TabSize = 1;
//                var reader = new SeekableStreamReader(new StringTextSource(cref));
//                var file = new SourceFile("", "", 0);
//                Location.Initialize(new List<SourceFile>(new [] { file }));
//                var module = new ModuleContainer(ctx);
//                module.DocumentationBuilder = new DocumentationBuilder(module);
//                var source_file = new CompilationSourceFile(module);
//                var report = new Report(ctx, errorReportPrinter);
//                var session = new ParserSession();
//                session.LocationsBag = new LocationsBag();
//                var parser = new ICSharpCode.NRefactory.MonoRava.CSharpParser(reader, source_file, report, session);
//                parser.Lexer.Line += initialLocation.Line - 1;
//                parser.Lexer.Column += initialLocation.Column - 1;
//                parser.Lexer.putback_char = Tokenizer.DocumentationXref;
//                parser.Lexer.parsing_generic_declaration_doc = true;
//                parser.parse();
//                if (report.Errors > 0) {
////					Report.Warning (1584, 1, mc.Location, "XML comment on `{0}' has syntactically incorrect cref attribute `{1}'",
////					                mc.GetSignatureForError (), cref);
//                }
				
//                var conversionVisitor = new ConversionVisitor(false, session.LocationsBag);
//                var docRef = conversionVisitor.ConvertXmlDoc(module.DocumentationBuilder);
//                CompilerCallableEntryPoint.Reset();
//                return docRef;
            return null;
			}
		
	}
}
