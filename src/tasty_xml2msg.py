from lxml.etree import parse
import re
from re import search, sub
from functools import reduce
from operator import add

#TODO: Should we enable nesting of testsuites?
def xml_to_corrector_string(xml_file, exc_name):
    """
    Assumes exactly one testsuite with possibly several testcases.
    """
    xml=parse(xml_file).getroot()
    subst_exc_name(xml,exc_name)
    main_test=xml.find('testsuite')
    failure_string=reduce(add,map(readify_failure,main_test.iter('failure')),'')
    return main_test.get('name')+': ' + ("correct" if not failure_string else failure_string)

def subst_exc_name(xml, exc_name):
    """replace occurences of the placeholder {S} in 
    test names with parameter exc_name.
    """
    for testsuite in xml.iter('testsuite'): 
        s=sub(r'{S}', exc_name, testsuite.get('name'))
        testsuite.set('name', s)

def readify_failure(failure):
    error_msg=sub(r'^Use --quickcheck-replay=(.*?)$','',str(failure.text),flags = re.MULTILINE| re.DOTALL)
    testcase_name=failure.getparent().get('name')
    return '\n' + ' Test "' + testcase_name + '" failed:\n' + error_msg
