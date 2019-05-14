from lxml.etree import parse
from re import search, sub
from functools import reduce, partial

def xml_to_corrector_string(xml_file, exc_name):
    xml=parse(xml_file).getroot()
    subst_exc_name(xml,exc_name)
    main_test=xml.findall('testsuite')[0]
    return main_test.get('name')+':' + reduce(assemble_suite_result_str, main_test.findall('testsuite'), '')

def subst_exc_name(xml, exc_name):
    """replace occurences of the placeholder {S} in 
    test names with parameter exc_name.
    """
    for testsuite in xml.iter('testsuite'):
        s=sub(r'{S}', exc_name, testsuite.get('name'))
        testsuite.set('name', s)

# Assume that one toplevel testsuite (can consist of further suites) represents one subexercise.
# For each toplevel testsuite one result string will be assembled for the tutor.

def readify_failure(test_name,failure):
    error_msg=str(failure.text)
    return '\n    Test ' + test_name + ' failed:\n      ' + error_msg

def assemble_suite_result_str(testsuite):
    test_name=testsuite.get('name')
    failure_string=reduce(partial(readify_failure,test_name),testsuite.iter('failed'),'')
    if not failure_string:
        testsuite_string=test_name + ' korrekt!'
    else:
        testsuite_string=test_name + ':' + failure_string
    return '\n  ' + testsuite_string



