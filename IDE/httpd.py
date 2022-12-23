# Python 3 server example
from http.server import BaseHTTPRequestHandler, HTTPServer
import time
import requests
import subprocess
import os
from urllib.parse import urlparse
from urllib.parse import parse_qs
import codecs
import json

from io import BytesIO

prolog_path = "/usr/local/bin/swipl"

hostName = "localhost" #"192.168.1.29"
serverPort = 8080

class MyServer(BaseHTTPRequestHandler):

    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()

        with open('prolog_ide.html', 'r') as file:
            data = file.read()
        self.wfile.write(bytes(data,"utf-8"))
            
        with open('winbox.bundle.min.js', 'r') as file:
            data = file.read()
        self.wfile.write(bytes("<script>" + data + "</script>","utf-8"))


    def do_POST(self):
        content_length = int(self.headers['Content-Length']) # <--- Gets the size of data
        print(content_length)

        self.send_response(200,"ok")
        self.send_header("Content-type", "text/html")
        self.end_headers()
       
        #https://stackoverflow.com/questions/68011018/how-to-parse-post-data-into-a-dictionary
        post_data = self.rfile.read(content_length) # <--- Gets the data itself
        data = post_data.decode("utf-8")
        result = parse_qs(data, strict_parsing=True,encoding='utf-8')
       
        for key in result:
            if len(result[key]) == 1:
                result[key] = result[key][0]

        code = result['code']

        with open("file.pl", "w") as text_file:
            text_file.write(code)

        prolog_goal = ""
        lines = code.split("\n");
        for line in lines:
            goal = line.split("% goal: ");
            if len(goal) == 2:
                prolog_goal = goal[1].rstrip(".").strip();

        if prolog_goal == "":
            send_back = json.dumps({"output": "","error": "Missing % goal: line in code."})
        else:
            code_to_run = f"['file.pl']. {prolog_goal}."

            result = subprocess.run(['swipl','-tty'],input=code_to_run,capture_output=True, encoding='UTF-8')
            print(result.stdout)
            print(result.stderr)
            print(result)
            send_back = json.dumps({"output":result.stdout, "error":result.stderr})
        self.wfile.write(bytes(send_back,"utf-8"))

if __name__ == "__main__":        
    webServer = HTTPServer((hostName, serverPort), MyServer)
    print("Server started http://%s:%s" % (hostName, serverPort))

    try:
        webServer.serve_forever()
    except KeyboardInterrupt:
        pass

    webServer.server_close()
    print("Server stopped.")


