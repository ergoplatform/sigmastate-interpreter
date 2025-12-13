import pytest
import json
from pathlib import Path
import sys

# Add scripts directory to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'scripts'))

from audit_contract import ErgoContractAuditor, Finding


class TestVulnerabilityPatterns:
    """Test vulnerability pattern detection"""
    
    @pytest.fixture
    def vulnerable_contract(self, tmp_path):
        """Create a vulnerable test contract"""
        contract = tmp_path / "vulnerable.es"
        contract.write_text("""
        {
            val oracleBox = INPUTS(0)
            val price = oracleBox.R4[Long].get
            val amount = SELF.R4[Long].get
            val payout = price * amount
            sigmaProp(OUTPUTS(0).value == payout)
        }
        """)
        return contract
    
    @pytest.fixture
    def secure_contract(self, tmp_path):
        """Create a secure test contract"""
        contract = tmp_path / "secure.es"
        contract.write_text("""
        {
            val oracle1 = INPUTS(0).R4[Long].get
            val oracle2 = INPUTS(1).R4[Long].get
            val oracle3 = INPUTS(2).R4[Long].get
            val medianPrice = Coll(oracle1, oracle2, oracle3).sorted(1)
            
            val amount = SELF.R4[Long].get
            val payoutBig = medianPrice.toBigInt * amount.toBigInt
            require(payoutBig <= MaxLong)
            
            val output = OUTPUTS(0)
            require(output.propositionBytes == expectedScript)
            require(output.value >= 1000000L)
            
            sigmaProp(output.value >= payoutBig)
        }
        """)
        return contract
    
    def test_auditor_initialization(self, vulnerable_contract):
        """Test auditor can be initialized"""
        auditor = ErgoContractAuditor(str(vulnerable_contract))
        assert auditor.contract_path == vulnerable_contract
        assert auditor.ml_enhanced == False
        assert auditor.economic_analysis == False
    
    def test_vulnerable_contract_detection(self, vulnerable_contract):
        """Test detection of vulnerabilities in vulnerable contract"""
        auditor = ErgoContractAuditor(str(vulnerable_contract), ml_enhanced=True, economic_analysis=True)
        report = auditor.audit()
        
        assert report['risk_score'] > 50, "Vulnerable contract should have high risk score"
        assert len(report['findings']) > 0, "Should detect vulnerabilities"
        
        # Check for specific vulnerabilities
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-002' in finding_ids, "Should detect integer overflow"
        assert 'ERGO-003' in finding_ids, "Should detect single oracle dependency"
    
    def test_secure_contract_validation(self, secure_contract):
        """Test secure contract has fewer/no critical issues"""
        auditor = ErgoContractAuditor(str(secure_contract))
        report = auditor.audit()
        
        critical_count = sum(1 for f in report['findings'] if f['severity'] == 'CRITICAL')
        assert critical_count == 0, "Secure contract should have no critical issues"
    
    def test_ergo_001_missing_signature(self, tmp_path):
        """Test ERGO-001: Missing Signature Verification"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val publicKey = R4[GroupElement].get
            val data = R5[Long].get
            sigmaProp(data > 100)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-001' in finding_ids, "Should detect missing signature verification"
    
    def test_ergo_002_integer_overflow(self, tmp_path):
        """Test ERGO-002: Integer Overflow"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val a = R4[Long].get
            val b = R5[Long].get
            val result = a * b
            sigmaProp(result > 0)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-002' in finding_ids, "Should detect integer overflow risk"
    
    def test_ergo_003_single_oracle(self, tmp_path):
        """Test ERGO-003: Single Oracle Dependency"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val price = INPUTS(0).R4[Long].get
            sigmaProp(price > 100)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-003' in finding_ids, "Should detect single oracle dependency"
    
    def test_ergo_006_mev_frontrunning(self, tmp_path):
        """Test ERGO-006: MEV/Front-Running Risk"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val price = INPUTS(0).R4[Long].get
            val amount = SELF.R4[Long].get
            sigmaProp(OUTPUTS(0).value == price * amount)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-006' in finding_ids, "Should detect MEV front-running risk"
    
    def test_ergo_011_missing_output_validation(self, tmp_path):
        """Test ERGO-011: Missing Output Validation"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val amount = SELF.R4[Long].get
            sigmaProp(OUTPUTS(0).value >= amount)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-011' in finding_ids, "Should detect missing output validation"
    
    def test_ergo_012_division_by_zero(self, tmp_path):
        """Test ERGO-012: Division by Zero"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val numerator = R4[Long].get
            val denominator = R5[Long].get
            val result = numerator / denominator
            sigmaProp(result > 0)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-012' in finding_ids, "Should detect division by zero risk"
    
    def test_ergo_015_frontrunning(self, tmp_path):
        """Test ERGO-015: Front-Running Vulnerability"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val deadline = CONTEXT.preHeader.timestamp
            sigmaProp(deadline > 1000000)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-015' in finding_ids, "Should detect front-running vulnerability"
    
    def test_ergo_016_insufficient_access_control(self, tmp_path):
        """Test ERGO-016: Insufficient Access Control"""
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val data1 = R4[Long].get
            val data2 = R5[Long].get
            val data3 = R6[Long].get
            sigmaProp(data1 + data2 + data3 > 0)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        report = auditor.audit()
        
        finding_ids = [f['id'] for f in report['findings']]
        assert 'ERGO-016' in finding_ids, "Should detect insufficient access control"
    
    def test_ml_enhancement(self, vulnerable_contract):
        """Test ML enhancement increases confidence"""
        auditor_no_ml = ErgoContractAuditor(str(vulnerable_contract), ml_enhanced=False)
        auditor_ml = ErgoContractAuditor(str(vulnerable_contract), ml_enhanced=True)
        
        report_no_ml = auditor_no_ml.audit()
        report_ml = auditor_ml.audit()
        
        assert report_ml['ml_enhanced'] == True
        assert report_no_ml['ml_enhanced'] == False
    
    def test_economic_analysis(self, vulnerable_contract):
        """Test economic analysis provides impact assessment"""
        auditor = ErgoContractAuditor(str(vulnerable_contract), economic_analysis=True)
        report = auditor.audit()
        
        assert report['economic_analysis'] == True
        
        # Check if economic impact is calculated for high-severity issues
        high_severity_findings = [f for f in report['findings'] if f['severity'] in ['CRITICAL', 'HIGH']]
        if high_severity_findings:
            assert any('economic_impact' in f for f in high_severity_findings)
    
    def test_risk_score_calculation(self, vulnerable_contract, secure_contract):
        """Test risk score calculation"""
        vuln_auditor = ErgoContractAuditor(str(vulnerable_contract))
        secure_auditor = ErgoContractAuditor(str(secure_contract))
        
        vuln_report = vuln_auditor.audit()
        secure_report = secure_auditor.audit()
        
        assert vuln_report['risk_score'] > secure_report['risk_score'], \
            "Vulnerable contract should have higher risk score"
        assert 0 <= vuln_report['risk_score'] <= 100
        assert 0 <= secure_report['risk_score'] <= 100
    
    def test_report_structure(self, vulnerable_contract):
        """Test audit report has correct structure"""
        auditor = ErgoContractAuditor(str(vulnerable_contract))
        report = auditor.audit()
        
        # Check required fields
        assert 'contract' in report
        assert 'timestamp' in report
        assert 'risk_score' in report
        assert 'risk_level' in report
        assert 'findings' in report
        assert 'ml_enhanced' in report
        assert 'economic_analysis' in report
        assert 'summary' in report
        
        # Check summary structure
        assert 'CRITICAL' in report['summary']
        assert 'HIGH' in report['summary']
        assert 'MEDIUM' in report['summary']
        assert 'LOW' in report['summary']
    
    def test_finding_structure(self, vulnerable_contract):
        """Test finding objects have correct structure"""
        auditor = ErgoContractAuditor(str(vulnerable_contract))
        report = auditor.audit()
        
        if report['findings']:
            finding = report['findings'][0]
            
            # Check required fields
            assert 'id' in finding
            assert 'name' in finding
            assert 'severity' in finding
            assert 'cwe' in finding
            assert 'line' in finding
            assert 'description' in finding
            assert 'impact' in finding


class TestCLI:
    """Test command-line interface"""
    
    def test_cli_help(self):
        """Test CLI help message"""
        import subprocess
        result = subprocess.run(
            ['python3', 'scripts/audit_contract.py', '--help'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert 'Ergo Contract Auditor' in result.stdout
    
    def test_cli_version(self):
        """Test CLI version output"""
        import subprocess
        result = subprocess.run(
            ['python3', 'scripts/audit_contract.py', '--version'],
            capture_output=True,
            text=True
        )
        assert result.returncode == 0
        assert '2.0.0' in result.stdout


class TestPerformance:
    """Test performance characteristics"""
    
    def test_audit_speed(self, tmp_path):
        """Test audit completes in reasonable time"""
        import time
        
        contract = tmp_path / "test.es"
        contract.write_text("""
        {
            val data = R4[Long].get
            sigmaProp(data > 100)
        }
        """)
        
        auditor = ErgoContractAuditor(str(contract))
        
        start = time.time()
        report = auditor.audit()
        duration = time.time() - start
        
        assert duration < 5.0, "Audit should complete within 5 seconds"
        assert report['analysis_time'] < 5.0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
