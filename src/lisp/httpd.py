#! /usr/bin/env python
# coding:utf-8

from BaseHTTPServer import HTTPServer
from BaseHTTPServer import BaseHTTPRequestHandler
import urlparse
import sys
import os
import subprocess
import json
import lctags_httpd_var

lctags_commnad = "lctags"



args = []
for arg in sys.argv:
    if arg.startswith( "-lctags=" ):
        lctags_commnad = arg[ len( "-lctags=" ): ]
    else:
        args.append( arg )

if len( args ) != 3:
    print( "usage: %s [-lctags=path] port dbpath" %os.path.basename( args[0] ) )
    print( "" )
    print( "   path: path of lctags" )
    print( "   port: port of httpd" )
    print( "   dbpath: path of lctags.sqlite3" )
    exit( 1 )

httpPort = int( args[1] )
dbPath = os.path.abspath( args[2] )
    
    

contentRoot = os.path.join( os.path.abspath( os.path.dirname( args[0] ) ), "html" )

suffix2TypeMap = { '.html': "text/html",
                   ".js": "text/json",
                   ".css": "text/css" }

def execCmd( cmd ):
    popen = subprocess.Popen( cmd, shell=True, stdout = subprocess.PIPE )
    return popen.communicate()[0]



jsonObj = json.loads( execCmd(
    "%s inq projDir --lctags-form json --lctags-db %s" %(lctags_commnad, dbPath ) ) )
projDir = jsonObj[ "lctags_result" ][ "projDir" ][0][ "path" ]
confInfo = { 'db': dbPath, 'projDir': projDir }
confId2InfoMap = { '0': confInfo }


class SimpleHttpd( BaseHTTPRequestHandler ):
    def __init__(self, *args ):
        BaseHTTPRequestHandler.__init__(self, *args)

    def exeLctags( self, confInfo, query ):
        api = query.get( "command" )[0]
        dbPath = confInfo[ "db" ]
        command = "%s --lctags-db '%s' inq %s " %(lctags_commnad, dbPath, api)
        apiInfo = lctags_httpd_var.apiInfoMap.get( api )
        if not apiInfo:
            return ""
        for param in apiInfo[ "param" ]:
            if isinstance( param, str ):
                if param.startswith( "?" ):
                    param = query.get( param[ 1: ] )[0]
            elif isinstance( param, list ):
                val = param[ 0 ]
                func = param[ 1 ]
                param = func( query.get( val[1:] )[0] )
            command = "%s %s" %(command, param )
    
        return execCmd( command )
        

    def responseLocalFile( self, path ):
        fileObj = open( os.path.join( contentRoot, path ) )
        self.send_response(200)
        suffix = os.path.splitext( path )[1]
        self.send_header("Content-Type", suffix2TypeMap.get( suffix ) )
        content = fileObj.read()
        fileObj.close()
        return content
        
    

    def do_GET(self):
        parsedUrl = urlparse.urlparse(self.path)
        path = parsedUrl[ 2 ]
        query = urlparse.parse_qs( parsedUrl[ 4 ] )
        if path == "/lctags":
            self.send_response(302)
            self.send_header("Location", "/lctags/contents/index.html")
            self.end_headers()
        elif path.startswith( "/lctags/contents/" ):
            content = self.responseLocalFile( path[ len( "/lctags/contents/" ): ] )

            self.send_header( "Content-Length", len( content ) )
            self.end_headers()
            
            self.wfile.write( content )
        elif path.startswith( "/lctags/gen/" ):
            content = self.responseLocalFile( path[ len( "/lctags/gen/" ): ] )

            confId = query.get( "confId" )[0]
            confInfo = confId2InfoMap.get( confId )
            query[ "projDir" ] = [ confInfo[ "projDir" ] ]
            for key, val in query.items():
                content = content.replace( "$%s$" %key, val[0] )

            self.send_header( "Content-Length", len( content ) )
            self.end_headers()
            self.wfile.write( content )
        elif path.startswith( "/lctags/start" ):
            cookie = query.get( "cookie" )[0]
            self.send_response(302)
            self.send_header("Location",
                             "/lctags/gen/file-list.html?confId=%s" %cookie)
            self.end_headers()
        elif path.startswith( "/lctags/inq" ):
            confId = query.get( "confId" )[0]
            confInfo = confId2InfoMap.get( confId )
            self.send_response(200)

            content = self.exeLctags( confInfo, query )
            self.send_header( "Content-Type", "text/json" )
            self.send_header( "Content-Length", len( content ) )
            self.end_headers()
            
            self.wfile.write( content )
        elif path.startswith( "/lctags/get" ):
            command = query.get( "command" )[0]

            if command == "cookies":
                obj = {}
                cookieList = []
                obj[ "list" ] = cookieList

                for confId, confInfo in confId2InfoMap.items():
                    info = {}
                    info[ "cookie" ] = confId
                    info[ "db" ] = confInfo.get( "db" )
                    cookieList.append( info )

                content = json.dumps( obj )

                self.send_response(200)
                self.send_header( "Content-Type", "text/json" )
                self.send_header( "Content-Length", len( content ) )
                self.end_headers()

                self.wfile.write( content )
        return


server = HTTPServer( ('', httpPort), SimpleHttpd )
print( "start lctags httpd -- %s %s" %(httpPort, dbPath ) )
server.serve_forever()
