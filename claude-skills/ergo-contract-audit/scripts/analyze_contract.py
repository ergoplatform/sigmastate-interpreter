#!/usr/bin/env python3
"""
ErgoScript Contract Analyzer
Analyzes ErgoScript contracts for structure, complexity, and potential issues.
"""

import re
import json
from typing import Dict, List, Tuple, Any

class ErgoContractAnalyzer:
    """Analyzes ErgoScript contracts for security and quality metrics."""
    
    def __init__(self, contract_code: str):
        self.code = contract_code
        self.lines = contract_code.split('\n')
        self.metrics = {}
        
    def analyze(self) -> Dict[str, Any]:
        """Perform comprehensive contract analysis."""
        return {
            'structure': self.analyze_structure(),
            'complexity': self.calculate_complexity(),
            'components': self.extract_components(),
            'metrics': self.calculate_metrics(),
            'warnings': self.generate_warnings()
        }
    
    def analyze_structure(self) -> Dict[str, Any]:
        """Analyze contract structure."""
        return {
            'total_lines': len(self.lines),
            'code_lines': len([l for l in self.lines if l.strip() and not l.strip().startswith('//')]),
            'comment_lines': len([l for l in self.lines if l.strip().startswith('//')]),
            'has_documentation': any('/**' in l or '*/' in l for l in self.lines),
            'braces_balanced': self.check_braces_balance()
        }
    
    def calculate_complexity(self) -> Dict[str, int]:
        """Calculate cyclomatic complexity and other metrics."""
        code = ' '.join(self.lines)
        
        # Count decision points
        if_count = len(re.findall(r'\bif\b', code))
        and_count = len(re.findall(r'&&', code))
        or_count = len(re.findall(r'\|\|', code))
        
        # Cyclomatic complexity = decision points + 1
        complexity = if_count + and_count + or_count + 1
        
        return {
            'cyclomatic': complexity,
            'if_statements': if_count,
            'and_operators': and_count,
            'or_operators': or_count,
            'nesting_depth': self.calculate_nesting_depth()
        }
    
    def extract_components(self) -> Dict[str, List[str]]:
        """Extract key contract components."""
        code = ' '.join(self.lines)
        
        return {
            'registers': re.findall(r'SELF\.R\d+', code),
            'outputs': re.findall(r'OUTPUTS\(\d+\)', code),
            'inputs': re.findall(r'INPUTS\(\d+\)', code),
            'context_vars': re.findall(r'(HEIGHT|SELF|OUTPUTS|INPUTS|getVar)', code),
            'crypto_ops': re.findall(r'(proveDlog|blake2b256|sha256|proveDHTuple)', code),
            'arithmetic_ops': re.findall(r'(\+|-|\*|/|%)', code)
        }
    
    def calculate_metrics(self) -> Dict[str, Any]:
        """Calculate various code quality metrics."""
        code = ' '.join(self.lines)
        
        return {
            'has_signature_check': bool(re.search(r'proveDlog|proveDHTuple', code)),
            'has_value_check': bool(re.search(r'\.value\s*[><=]', code)),
            'has_height_check': bool(re.search(r'HEIGHT\s*[><=]', code)),
            'uses_bigint': bool(re.search(r'\.toBigInt', code)),
            'has_require': bool(re.search(r'\brequire\b', code)),
            'has_error_messages': bool(re.search(r'require\s*\([^,]+,\s*"', code)),
            'token_operations': len(re.findall(r'\.tokens', code))
        }
    
    def generate_warnings(self) -> List[Dict[str, str]]:
        """Generate warnings for potential issues."""
        warnings = []
        code = ' '.join(self.lines)
        
        # Check for common issues
        if not re.search(r'proveDlog|proveDHTuple', code):
            warnings.append({
                'severity': 'HIGH',
                'message': 'No signature verification detected',
                'recommendation': 'Add proveDlog() or proveDHTuple() for access control'
            })
        
        if re.search(r'\+\s*\d+|\d+\s*\+', code) and not re.search(r'toBigInt', code):
            warnings.append({
                'severity': 'MEDIUM',
                'message': 'Arithmetic operations without overflow protection',
                'recommendation': 'Use .toBigInt for calculations that might overflow'
            })
        
        if not re.search(r'\brequire\b', code):
            warnings.append({
                'severity': 'LOW',
                'message': 'No explicit require() statements',
                'recommendation': 'Use require() for input validation with error messages'
            })
        
        if re.search(r'HEIGHT\s*==', code):
            warnings.append({
                'severity': 'MEDIUM',
                'message': 'Exact height comparison detected',
                'recommendation': 'Use >= or <= instead of == for height checks'
            })
        
        return warnings
    
    def check_braces_balance(self) -> bool:
        """Check if braces are balanced."""
        open_count = self.code.count('{')
        close_count = self.code.count('}')
        return open_count == close_count
    
    def calculate_nesting_depth(self) -> int:
        """Calculate maximum nesting depth."""
        max_depth = 0
        current_depth = 0
        
        for char in self.code:
            if char == '{':
                current_depth += 1
                max_depth = max(max_depth, current_depth)
            elif char == '}':
                current_depth -= 1
        
        return max_depth


def analyze_contract(contract_code: str) -> Dict[str, Any]:
    """Main function to analyze a contract."""
    analyzer = ErgoContractAnalyzer(contract_code)
    return analyzer.analyze()


def format_analysis_report(analysis: Dict[str, Any]) -> str:
    """Format analysis results as a readable report."""
    report = []
    report.append("=" * 60)
    report.append("ERGO CONTRACT ANALYSIS REPORT")
    report.append("=" * 60)
    
    # Structure
    report.append("\n📊 STRUCTURE:")
    for key, value in analysis['structure'].items():
        report.append(f"  {key}: {value}")
    
    # Complexity
    report.append("\n🔢 COMPLEXITY:")
    for key, value in analysis['complexity'].items():
        report.append(f"  {key}: {value}")
    
    # Components
    report.append("\n🔧 COMPONENTS:")
    for key, value in analysis['components'].items():
        if value:
            report.append(f"  {key}: {len(value)} found")
    
    # Metrics
    report.append("\n✅ METRICS:")
    for key, value in analysis['metrics'].items():
        status = "✓" if value else "✗"
        report.append(f"  {status} {key}: {value}")
    
    # Warnings
    if analysis['warnings']:
        report.append("\n⚠️  WARNINGS:")
        for warning in analysis['warnings']:
            report.append(f"\n  [{warning['severity']}] {warning['message']}")
            report.append(f"  → {warning['recommendation']}")
    
    report.append("\n" + "=" * 60)
    return "\n".join(report)


if __name__ == "__main__":
    # Example usage
    example_contract = """
    {
      val ownerPubKey = SELF.R4[GroupElement].get
      val minBid = SELF.R5[Long].get
      
      val bid = OUTPUTS(0).value
      val validBid = bid >= minBid
      
      sigmaProp(proveDlog(ownerPubKey) && validBid)
    }
    """
    
    analysis = analyze_contract(example_contract)
    print(format_analysis_report(analysis))
