import * as ts from 'typescript';
import * as fs from 'fs';
import * as path from 'path';

// Tipos para mapear TypeScript -> Delphi
interface DelphiType {
  name: string;
  fields: DelphiField[];
  documentation?: string;
  extends?: string;
}

interface DelphiField {
  name: string;
  type: string;
  optional: boolean;
  documentation?: string;
}

class LSPCodeGenerator {
  private program: ts.Program;
  private checker: ts.TypeChecker;
  private delphiTypes: DelphiType[] = [];

  constructor(filePaths: string[]) {
    this.program = ts.createProgram(filePaths, {
      target: ts.ScriptTarget.ES2020,
      module: ts.ModuleKind.CommonJS,
    });
    this.checker = this.program.getTypeChecker();
  }

  // Método principal para processar arquivos
  public generateDelphiCode(): void {
    for (const sourceFile of this.program.getSourceFiles()) {
      if (!sourceFile.isDeclarationFile) {
        this.visitNode(sourceFile);
      }
    }

    this.generateDelphiUnits();
  }

  // Visita cada nó da AST
  private visitNode(node: ts.Node): void {
    if (ts.isInterfaceDeclaration(node)) {
      this.processInterface(node);
    }
    
    ts.forEachChild(node, child => this.visitNode(child));
  }

  // Processa uma interface TypeScript
  private processInterface(node: ts.InterfaceDeclaration): void {
    const delphiType: DelphiType = {
      name: node.name.text,
      fields: [],
      documentation: this.getDocumentation(node),
      extends: this.getExtendsClause(node)
    };

    // Processar membros da interface
    for (const member of node.members) {
      if (ts.isPropertySignature(member)) {
        const field = this.processProperty(member);
        if (field) {
          delphiType.fields.push(field);
        }
      }
    }

    this.delphiTypes.push(delphiType);
  }

  // Processa uma propriedade da interface
  private processProperty(property: ts.PropertySignature): DelphiField | null {
    if (!property.name || !property.type) return null;

    const name = property.name.getText();
    const isOptional = !!property.questionToken;
    const delphiType = this.mapTypeScriptToDelphiType(property.type);
    const documentation = this.getDocumentation(property);

    return {
      name: this.toPascalCase(name),
      type: delphiType,
      optional: isOptional,
      documentation
    };
  }

  // Mapeia tipos TypeScript para Delphi
  private mapTypeScriptToDelphiType(typeNode: ts.TypeNode): string {
    switch (typeNode.kind) {
      case ts.SyntaxKind.StringKeyword:
        return 'string';
      case ts.SyntaxKind.NumberKeyword:
        return 'Integer'; // ou Double, dependendo do contexto
      case ts.SyntaxKind.BooleanKeyword:
        return 'Boolean';
      case ts.SyntaxKind.ArrayType:
        const arrayType = typeNode as ts.ArrayTypeNode;
        const elementType = this.mapTypeScriptToDelphiType(arrayType.elementType);
        return `TArray<${elementType}>`;
      case ts.SyntaxKind.TypeReference:
        const typeRef = typeNode as ts.TypeReferenceNode;
        return typeRef.typeName.getText();
      case ts.SyntaxKind.UnionType:
        // Para union types, usar Variant ou criar enum quando possível
        return 'Variant';
      default:
        return 'Variant'; // Fallback
    }
  }

  // Extrai documentação JSDoc
  private getDocumentation(node: ts.Node): string | undefined {
    const symbol = this.checker.getSymbolAtLocation(node);
    if (symbol) {
      return ts.displayPartsToString(symbol.getDocumentationComment(this.checker));
    }
    return undefined;
  }

  // Extrai cláusula extends
  private getExtendsClause(node: ts.InterfaceDeclaration): string | undefined {
    if (node.heritageClauses) {
      for (const clause of node.heritageClauses) {
        if (clause.token === ts.SyntaxKind.ExtendsKeyword) {
          return clause.types[0]?.expression.getText();
        }
      }
    }
    return undefined;
  }

  // Converte para PascalCase
  private toPascalCase(str: string): string {
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  // Gera as units Delphi
  private generateDelphiUnits(): void {
    let unitContent = this.generateUnitHeader();
    
    for (const delphiType of this.delphiTypes) {
      unitContent += this.generateRecord(delphiType);
    }

    unitContent += this.generateUnitFooter();

    // Salvar arquivo
    const outputPath = path.join(__dirname, '../output/LSPTypes.pas');
    fs.mkdirSync(path.dirname(outputPath), { recursive: true });
    fs.writeFileSync(outputPath, unitContent);

    console.log(`Código Delphi gerado: ${outputPath}`);
    console.log(`${this.delphiTypes.length} tipos processados`);
  }

  private generateUnitHeader(): string {
    return `unit LSPTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type
`;
  }

  private generateRecord(delphiType: DelphiType): string {
    let result = '';
    
    if (delphiType.documentation) {
      result += `  // ${delphiType.documentation}\n`;
    }
    
    result += `  T${delphiType.name} = record\n`;
    
    for (const field of delphiType.fields) {
      if (field.documentation) {
        result += `    // ${field.documentation}\n`;
      }
      result += `    ${field.name}: ${field.type};\n`;
    }
    
    result += `  end;\n\n`;
    
    return result;
  }

  private generateUnitFooter(): string {
    return `
implementation

end.
`;
  }
}

// Uso do gerador
function main() {
  // Caminho para os arquivos da especificação LSP
  const lspFiles = [
    './lsp-specification/types.ts',  // Ajustar conforme estrutura
    './lsp-specification/protocol.ts'
  ];

  const generator = new LSPCodeGenerator(lspFiles);
  generator.generateDelphiCode();
}

// Executar se chamado diretamente
if (require.main === module) {
  main();
}