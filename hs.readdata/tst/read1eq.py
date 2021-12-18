evento = 2639; dist=200e3; days=60;
import json
salida = sp.check_output(['../dfexe','--event',str(evento),'--dist',str(dist),'--days',str(days)])
import subprocess as sp
salida = sp.check_output(['../dfexe','--event',str(evento),'--dist',str(dist),'--days',str(days)])
json.loads(salida)
json.loads(salida)
%hist -f read1eq.py
