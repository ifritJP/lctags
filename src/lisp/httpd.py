#! /usr/bin/env python3
# coding:utf-8

from http.server import HTTPServer
from http.server import BaseHTTPRequestHandler
import urllib.parse
import sys
import os
import subprocess
import json
import lctags_httpd_var


lctags_command = "lctags"
arg_list = []
for val in sys.argv:
    if val.startswith("-lctags="):
        lctags_command = val[len("-lctags="):]
    else:
        arg_list.append(val)

if len(arg_list) not in (2, 3):
    print("usage: %s [-lctags=path] port [dbpath]" % os.path.basename(arg_list[0]))
    print("")
    print("   path: path of lctags")
    print("   port: port of httpd")
    print("   dbpath: optional default path of lctags.sqlite3")
    sys.exit(1)

http_port = int(arg_list[1])
default_db_path = None
if len(arg_list) == 3:
    default_db_path = os.path.abspath(arg_list[2])

content_root = os.path.join(os.path.abspath(os.path.dirname(arg_list[0])), "html")

suffix_to_type_map = {
    ".html": "text/html",
    ".js": "text/javascript",
    ".css": "text/css",
}


def run_lctags(args, cwd=None):
    print( "run_lctags", cwd, args )
    proc = subprocess.run(
        [lctags_command] + args,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        cwd=cwd,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.decode("utf-8", errors="ignore")
    return proc.stdout, None


def to_first(query, key, default=None):
    val = query.get(key)
    if not val:
        return default
    return val[0]


def to_bool(query, key):
    return to_first(query, key) is not None


def normalize_path(path):
    if not path:
        return None
    return os.path.abspath(path)


def normalize_proj_dir(path):
    if path is None:
        return ""
    path = str(path).strip()
    if path == "":
        return ""
    path = os.path.abspath(path)
    if path != "/":
        path = path.rstrip("/")
    return path


conf_id_to_info = {}
conf_key_to_id = {}
last_cookie = None


def make_conf_key(db_path, target, conf_path):
    return "%s$%s$%s" % (db_path or "", target or "", conf_path or "")


def resolve_run_cwd(info):
    proj_dir = info.get("projDir") if info else None
    if proj_dir:
        return proj_dir
    db_path = info.get("db") if info else None
    if db_path:
        return os.path.dirname(db_path)
    return None


def query_proj_dir(info):
    args = ["inq", "projDir", "--lctags-form", "json", "--lctags-db", info["db"]]
    if info.get("target"):
        args.extend(["--lctags-target", info["target"]])
    if info.get("conf"):
        args.extend(["--lctags-conf", info["conf"]])
    out, err = run_lctags(args, cwd=resolve_run_cwd(info))
    if out is None:
        return None, err
    try:
        payload = json.loads(out.decode("utf-8", errors="ignore"))
        proj_dir = normalize_proj_dir(payload["lctags_result"]["projDir"][0]["path"])
        return proj_dir, None
    except Exception as ex:
        return None, str(ex)


def prepare_lctags(info):
    args = ["prepare", "--lctags-db", info["db"]]
    if info.get("target"):
        args.extend(["--lctags-target", info["target"]])
    if info.get("conf"):
        args.extend(["--lctags-conf", info["conf"]])
    out, err = run_lctags(args, cwd=resolve_run_cwd(info))
    if out is None:
        return err
    return None


def make_cookie(db_path, target=None, conf_path=None, target_symbol=None, target_ns_id=None):
    global last_cookie

    if not db_path:
        if default_db_path:
            db_path = default_db_path
        else:
            return None, "db is required"

    db_path = normalize_path(db_path)
    conf_path = normalize_path(conf_path) if conf_path else None
    key = make_conf_key(db_path, target, conf_path)

    cookie = conf_key_to_id.get(key)
    if cookie is not None:
        info = conf_id_to_info[cookie]
        if target_symbol is not None:
            info["targetSymbol"] = target_symbol
        if target_ns_id is not None:
            info["targetNsId"] = target_ns_id
        last_cookie = cookie
        return cookie, None

    cookie = str(len(conf_id_to_info))
    info = {
        "db": db_path,
        "target": target,
        "conf": conf_path,
        "cookie": cookie,
        "targetSymbol": target_symbol,
        "targetNsId": target_ns_id,
    }

    proj_dir, err = query_proj_dir(info)
    if err is not None:
        return None, err
    info["projDir"] = proj_dir

    err = prepare_lctags(info)
    if err is not None:
        return None, err

    conf_id_to_info[cookie] = info
    conf_key_to_id[key] = cookie
    last_cookie = cookie
    return cookie, None


if default_db_path:
    c, e = make_cookie(default_db_path)
    if e is not None:
        print("warning: failed to initialize default db -- %s" % e)


class SimpleHttpd(BaseHTTPRequestHandler):
    def write_body(self, content):
        if isinstance(content, str):
            content = content.encode("utf-8")
        self.wfile.write(content)

    def send_json(self, status, payload):
        content = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "text/json")
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.write_body(content)

    def send_error_json(self, status, message):
        self.send_json(status, {"error": message})

    def response_local_file(self, path):
        full_path = os.path.abspath(os.path.join(content_root, path))
        if not full_path.startswith(content_root):
            return None, "invalid path"
        if not os.path.isfile(full_path):
            return None, "file not found"
        with open(full_path, "rb") as file_obj:
            content = file_obj.read()
        return content, None

    def build_lctags_query_args(self, conf_info, query):
        api = to_first(query, "command")
        api_info = lctags_httpd_var.apiInfoMap.get(api)
        if not api_info:
            return None, "not found command -- %s" % api

        command = api_info.get("command", api)
        args = ["inq", command]

        if conf_info.get("db"):
            args.extend(["--lctags-db", conf_info["db"]])
        if conf_info.get("target"):
            args.extend(["--lctags-target", conf_info["target"]])
        if conf_info.get("conf"):
            args.extend(["--lctags-conf", conf_info["conf"]])

        for param in api_info.get("param", []):
            if isinstance(param, str):
                if param.startswith("?"):
                    key = param[1:]
                    val = to_first(query, key)
                    if val is not None:
                        args.append(val)
                else:
                    args.append(param)
            elif isinstance(param, list) and len(param) == 2:
                query_key = param[0]
                convert_func = param[1]
                if not isinstance(query_key, str) or not query_key.startswith("?"):
                    return None, "illegal param spec"
                raw_val = to_first(query, query_key[1:])
                if raw_val is None:
                    return None, "query parameter is required -- %s" % query_key[1:]
                args.append(convert_func(raw_val))
            else:
                return None, "illegal param format"

        return {"args": args, "api": api, "apiInfo": api_info}, None

    def add_open_target(self, payload, api_info):
        target_name = api_info.get("openTarget")
        if not target_name:
            return payload
        result_root = payload.get("lctags_result")
        if not isinstance(result_root, dict):
            return payload
        target_list = result_root.get(target_name)
        if not isinstance(target_list, list):
            return payload

        picked = None
        for item in target_list:
            info = item.get("info") if isinstance(item, dict) else None
            if not isinstance(info, dict):
                continue
            if picked is None:
                picked = info
            if info.get("hasBodyFlag") is True:
                picked = info
                break

        if picked is not None:
            payload["openTarget"] = {
                "path": picked.get("path"),
                "line": picked.get("line"),
                "column": picked.get("column"),
            }
        return payload

    def handle_inq(self, query):
        conf_id = to_first(query, "confId")
        if conf_id is None:
            self.send_error_json(404, "not found cookie")
            return

        conf_info = conf_id_to_info.get(conf_id)
        if conf_info is None:
            self.send_error_json(404, "not found cookie")
            return

        req, err = self.build_lctags_query_args(conf_info, query)
        if err is not None:
            self.send_error_json(500, err)
            return

        out, run_err = run_lctags(req["args"], cwd=resolve_run_cwd(conf_info))
        if out is None:
            self.send_error_json(500, run_err)
            return

        try:
            payload = json.loads(out.decode("utf-8", errors="ignore"))
            payload = self.add_open_target(payload, req["apiInfo"])
            content = json.dumps(payload, ensure_ascii=False).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "text/json")
            self.send_header("Content-Length", str(len(content)))
            self.end_headers()
            self.write_body(content)
        except Exception:
            self.send_response(200)
            self.send_header("Content-Type", "text/json")
            self.send_header("Content-Length", str(len(out)))
            self.end_headers()
            self.write_body(out)

    def handle_get(self, query):
        command = to_first(query, "command")
        if command != "cookies":
            self.send_error_json(500, "not found command -- %s" % command)
            return

        cookie_list = []
        for conf_id in sorted(conf_id_to_info.keys(), key=lambda x: int(x)):
            info = conf_id_to_info[conf_id]
            cookie_list.append(
                {
                    "cookie": conf_id,
                    "db": info.get("db"),
                    "target": info.get("target"),
                    "conf": info.get("conf"),
                    "targetSymbol": info.get("targetSymbol"),
                    "targetNsId": info.get("targetNsId"),
                }
            )

        self.send_json(200, {"list": cookie_list})

    def handle_start(self, query):
        global last_cookie

        cookie = to_first(query, "cookie")
        jump_call_graph = to_bool(query, "jumpCallGraph")
        jump_last_symbol = to_bool(query, "jumpLastSymbol")

        if jump_last_symbol:
            jump_call_graph = True
            cookie = last_cookie

        if cookie is None:
            db = to_first(query, "db")
            target = to_first(query, "target")
            conf = to_first(query, "conf")
            target_symbol = to_first(query, "targetSymbol")
            target_ns_id_txt = to_first(query, "targetNsId")
            target_ns_id = None
            if target_ns_id_txt is not None:
                try:
                    target_ns_id = int(target_ns_id_txt)
                except ValueError:
                    target_ns_id = None

            cookie, err = make_cookie(db, target, conf, target_symbol, target_ns_id)
            if err is not None:
                self.send_error_json(500, err)
                return

        conf_info = conf_id_to_info.get(str(cookie))
        if conf_info is None:
            self.send_error_json(404, "not found cookie")
            return

        target_symbol = conf_info.get("targetSymbol")
        target_ns_id = conf_info.get("targetNsId")

        if jump_call_graph and target_symbol and target_ns_id is not None:
            location = "/lctags/gen/func-call-graph.html?confId=%s&nsId=%s&name=%s" % (
                urllib.parse.quote(str(cookie)),
                urllib.parse.quote(str(target_ns_id)),
                urllib.parse.quote(str(target_symbol)),
            )
        else:
            location = "/lctags/gen/file-list.html?confId=%s" % urllib.parse.quote(str(cookie))

        self.send_response(302)
        self.send_header("Location", location)
        self.end_headers()

    def handle_gen(self, path, query):
        rel_path = path[len("/lctags/gen/"):]
        content, err = self.response_local_file(rel_path)
        if err is not None:
            self.send_error_json(404, err)
            return

        conf_id = to_first(query, "confId")
        conf_info = conf_id_to_info.get(conf_id)
        if conf_info is None:
            self.send_error_json(404, "not found cookie")
            return

        rep_map = dict((k, v[0]) for k, v in query.items() if len(v) > 0)
        rep_map["projDir"] = conf_info.get("projDir", "")

        for key, val in rep_map.items():
            content = content.replace(
                ("$%s$" % key).encode("utf-8"),
                str(val).encode("utf-8"),
            )

        ext = os.path.splitext(rel_path)[1]
        content_type = suffix_to_type_map.get(ext, "text/html")

        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.write_body(content)

    def handle_contents(self, path):
        rel_path = path[len("/lctags/contents/"):]
        content, err = self.response_local_file(rel_path)
        if err is not None:
            self.send_error_json(404, err)
            return

        ext = os.path.splitext(rel_path)[1]
        content_type = suffix_to_type_map.get(ext, "application/octet-stream")
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(content)))
        self.end_headers()
        self.write_body(content)

    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        path = parsed.path
        query = urllib.parse.parse_qs(parsed.query)

        if path == "/lctags":
            content, err = self.response_local_file("index.html")
            if err is not None:
                self.send_error_json(404, err)
                return
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Content-Length", str(len(content)))
            self.end_headers()
            self.write_body(content)
            return

        if path.startswith("/lctags/contents/"):
            self.handle_contents(path)
            return

        if path.startswith("/lctags/gen/"):
            self.handle_gen(path, query)
            return

        if path.startswith("/lctags/start"):
            self.handle_start(query)
            return

        if path.startswith("/lctags/inq"):
            self.handle_inq(query)
            return

        if path.startswith("/lctags/get"):
            self.handle_get(query)
            return

        self.send_error_json(404, "not found")


server = HTTPServer(("", http_port), SimpleHttpd)
if default_db_path:
    print("start lctags httpd -- %s %s" % (http_port, default_db_path))
else:
    print("start lctags httpd -- %s" % http_port)
server.serve_forever()
