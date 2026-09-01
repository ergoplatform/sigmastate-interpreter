#!/usr/bin/env python3
"""
ErgoScript Pattern Checker
Checks ErgoScript contracts for known vulnerability patterns and anti-patterns.
"""

import re
import json
from typing import List, Dict, Any
from pathlib import Path

class PatternChecker:
    """Checks contracts against known vulnerability patterns."""
    
    def __init__(self, patterns_file: str = None):
        """Initialize with vulnerability patterns."""
        if patterns_file and Path(patterns_file).exists():
            with open(patterns_file, 'r') as f:
                self.patterns = json.load(f)
        else:
            self.patterns = self.get_default_patterns()
    
    def check_contract(self, contract_code: str) -> List[Dict[str, Any]]:
        """Check contract against all patterns."""
        findings = []
        
        for category, patterns in self.patterns.items():
            for pattern in patterns:
                if self.matches_pattern(contract_code, pattern):
                    findings.append({
                        'category': category,
                        'severity': pattern['severity'],
                        'name': pattern['name'],
                        'description': pattern['description'],
                        'recommendation': pattern['recommendation'],
                        'cwe': pattern.get('cwe', 'N/A')
                    })
        
        return findings
    
    def matches_pattern(self, code: str, pattern: Dict[str, Any]) -> bool:
        """Check if code matches a vulnerability pattern."""
        if 'regex' in pattern:
            return bool(re.search(pattern['regex'], code, re.IGNORECASE))
        
        if 'conditions' in pattern:
            return all(
                self.check_condition(code, cond) 
                for cond in pattern['conditions']
            )
        
        return False
    
    def check_condition(self, code: str, condition: Dict[str, Any]) -> bool:
        """Check a single condition."""
        if condition['type'] == 'has':
            return condition['pattern'] in code
        elif condition['type'] == 'missing':
            return condition['pattern'] not in code
        elif condition['type'] == 'regex':
            return bool(re.search(condition['pattern'], code))
        return False
    
    @staticmethod
    def get_default_patterns() -> Dict[str, List[Dict[str, Any]]]:
        """Get default vulnerability patterns."""
        return {
            "access_control": [
                {
                    "name": "Missing Signature Verification",
                    "severity": "CRITICAL",
                    "description": "Contract does not verify signatures for privileged operations",
                    "regex": r"(?!.*proveDlog)(?!.*proveDHTuple).*OUTPUTS",
                    "recommendation": "Add proveDlog() or proveDHTuple() to verify signatures",
                    "cwe": "CWE-862"
                },
                {
                    "name": "Unused Owner Key",
                    "severity": "HIGH",
                    "description": "Owner public key is extracted but never used",
                    "conditions": [
                        {"type": "has", "pattern": "ownerPubKey"},
                        {"type": "missing", "pattern": "proveDlog(ownerPubKey)"}
                    ],
                    "recommendation": "Use ownerPubKey in proveDlog() check or remove if unnecessary"
                }
            ],
            "arithmetic": [
                {
                    "name": "Integer Overflow Risk",
                    "severity": "HIGH",
                    "description": "Arithmetic operations without overflow protection",
                    "conditions": [
                        {"type": "regex", "pattern": r"\+|\-|\*"},
                        {"type": "missing", "pattern": "toBigInt"}
                    ],
                    "recommendation": "Use .toBigInt for calculations that might overflow"
                },
                {
                    "name": "Division Without Zero Check",
                    "severity": "MEDIUM",
                    "description": "Division operation without checking for zero divisor",
                    "regex": r"/\s*\w+(?!.*require.*!=\s*0)",
                    "recommendation": "Add require(divisor != 0) before division"
                }
            ],
            "context_validation": [
                {
                    "name": "Missing Output Validation",
                    "severity": "HIGH",
                    "description": "Output boxes not validated for script or value",
                    "conditions": [
                        {"type": "has", "pattern": "OUTPUTS"},
                        {"type": "missing", "pattern": "propositionBytes"}
                    ],
                    "recommendation": "Validate OUTPUTS propositionBytes to prevent fund redirection"
                },
                {
                    "name": "Missing Input Validation",
                    "severity": "MEDIUM",
                    "description": "No validation of input values or tokens",
                    "conditions": [
                        {"type": "has", "pattern": "INPUTS"},
                        {"type": "missing", "pattern": "require"}
                    ],
                    "recommendation": "Add require() statements to validate inputs"
                }
            ],
            "cryptographic": [
                {
                    "name": "Weak Randomness",
                    "severity": "HIGH",
                    "description": "Using predictable randomness source",
                    "regex": r"HEIGHT\s*%|SELF\.id\s*%",
                    "recommendation": "Use VRF or commit-reveal scheme for critical randomness"
                },
                {
                    "name": "Timestamp Dependence",
                    "severity": "MEDIUM",
                    "description": "Critical logic depends on exact timestamp",
                    "regex": r"HEIGHT\s*==\s*\d+",
                    "recommendation": "Use >= or <= for height checks instead of exact equality"
                }
            ],
            "logic": [
                {
                    "name": "Front-Running Vulnerability",
                    "severity": "HIGH",
                    "description": "Public auction or trade without protection",
                    "conditions": [
                        {"type": "has", "pattern": "auction"},
                        {"type": "missing", "pattern": "commit"}
                    ],
                    "recommendation": "Implement commit-reveal scheme or time locks"
                },
                {
                    "name": "Single Oracle Dependency",
                    "severity": "MEDIUM",
                    "description": "Relying on single oracle without validation",
                    "conditions": [
                        {"type": "regex", "pattern": r"oracle.*R\d+"},
                        {"type": "missing", "pattern": "median|average"}
                    ],
                    "recommendation": "Use multiple oracles and aggregate prices"
                }
            ],
            "code_quality": [
                {
                    "name": "Missing Error Messages",
                    "severity": "LOW",
                    "description": "require() statements without error messages",
                    "regex": r'require\s*\([^,]+\)\s*(?!,\s*")',
                    "recommendation": "Add descriptive error messages to all require() calls"
                },
                {
                    "name": "No Documentation",
                    "severity": "LOW",
                    "description": "Contract lacks documentation comments",
                    "conditions": [
                        {"type": "missing", "pattern": "/**"},
                        {"type": "missing", "pattern": "//"}
                    ],
                    "recommendation": "Add documentation explaining contract purpose and parameters"
                }
            ]
        }


def check_patterns(contract_code: str, patterns_file: str = None) -> List[Dict[str, Any]]:
    """Main function to check contract patterns."""
    checker = PatternChecker(patterns_file)
    return checker.check_contract(contract_code)


def format_findings_report(findings: List[Dict[str, Any]]) -> str:
    """Format findings as a readable report."""
    if not findings:
        return "✅ No vulnerability patterns detected!"
    
    report = []
    report.append("=" * 60)
    report.append("VULNERABILITY PATTERN CHECK RESULTS")
    report.append("=" * 60)
    
    # Group by severity
    by_severity = {}
    for finding in findings:
        severity = finding['severity']
        if severity not in by_severity:
            by_severity[severity] = []
        by_severity[severity].append(finding)
    
    # Display in order of severity
    for severity in ['CRITICAL', 'HIGH', 'MEDIUM', 'LOW']:
        if severity in by_severity:
            report.append(f"\n🚨 {severity} SEVERITY ({len(by_severity[severity])} issues):")
            for i, finding in enumerate(by_severity[severity], 1):
                report.append(f"\n  {i}. {finding['name']}")
                report.append(f"     Category: {finding['category']}")
                report.append(f"     {finding['description']}")
                report.append(f"     → {finding['recommendation']}")
                if finding['cwe'] != 'N/A':
                    report.append(f"     CWE: {finding['cwe']}")
    
    report.append("\n" + "=" * 60)
    report.append(f"Total Issues Found: {len(findings)}")
    report.append("=" * 60)
    
    return "\n".join(report)


if __name__ == "__main__":
    # Example usage
    example_contract = """
    {
      val ownerPubKey = SELF.R4[GroupElement].get
      val amount = SELF.R5[Long].get
      
      val output = OUTPUTS(0).value
      val total = amount + output
      
      sigmaProp(total > 0)
    }
    """
    
    findings = check_patterns(example_contract)
    print(format_findings_report(findings))
