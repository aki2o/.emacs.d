/// <reference path="typings/node/node.d.ts" />
/// <reference path="typings/request/request.d.ts" />
/// <reference path="typings/jquery/jquery.d.ts" />
/// <reference path="lib/basic.ts" />
/// <reference path="lib/football.ts" />
/// <reference path="lib/volleyball.ts" />

import request = require("request");
import jsdom = require("jsdom");
import fs = require("fs");
import cp = require("child_process");
import opts = require("opts");
import basic = require("lib/basic");
import football = require("lib/football");
import volleyball = require("lib/volleyball");


var JQUERY_URL = "http://ajax.googleapis.com/ajax/libs/jquery/1.8.3/jquery.js";
var confs: Object[] = [];
var scheds: basic.Schedule[] = [];
var srcdir = __dirname+"/source";
var cachefile = __dirname+"/.cache";
var orgfile = __dirname+"/../main.org";


opts.parse([{ short: "v",
              long: "verbose",
              description: "Show verbose.",
              value: false, },
            { long: "dry-run",
              description: "Just fetch schedule.",
              value: false, },
            { short: "s",
              long: "source",
              description: "Specify source.",
              value: true, }],
           true);


if ( opts.get("verbose") ) basic.setDebugOn();
var dryrun = opts.get("dry-run");

var srcpath = opts.get("source");
if ( srcpath ) {
    load_source(srcpath);
    next_collect_schedule();
}
else {
    fs.readdir(srcdir, (err, files) => {
        if ( err ) {
            console.error("Failed access '"+srcdir+"' : "+err);
            process.exit(1);
        }
        
        files.filter( (f) => {
            return fs.statSync(srcdir+"/"+f).isFile() && /.+\.json$/.test(f);
        }).forEach( (f) => {
            load_source(srcdir+"/"+f);
        });
        
        next_collect_schedule();
    });
}



function load_source(fpath: string) {
    var src = eval("("+fs.readFileSync(fpath, "utf8")+")");
    var currconfs:Object[] = src["entries"];
    currconfs.forEach( (c) => {
        confs.push(c);
    });
    basic.debug("Loaded "+fpath);
}

function next_collect_schedule(): void {
    var c = confs.pop();
    c ? collect_schedule(c) : regist_schedule();
}

function collect_schedule(conf: Object): void {
    var url: string = conf["url"];
    basic.debug("Start fetch '"+url+"' ...");
    request.get(url, { pool: { agent: false } }, (err, res, body) => {
        if ( err ) {
            console.error("Failed fetch '"+url+"' : "+err);
            next_collect_schedule();
            return;
        }
        if ( res.statusCode != 200 ) {
            console.error("Failed fetch '"+url+"' : Returned status is "+res.statusCode);
            next_collect_schedule();
            return;
        }

        jsdom.env({ html: body, scripts: [ JQUERY_URL ], done: (err, window) => {
            if ( err ) {
                console.error("Failed do jsdom : "+err);
                next_collect_schedule();
                return;
            }
            var f = eval("( function (c, m) { return new m."+conf["parser"]+"(c); } )");
            var m = conf["module"] == "football"   ? football
                :   conf["module"] == "volleyball" ? volleyball
                :                                    undefined;
            var parser: basic.Parser = (f)(conf, m);
            var currscheds: basic.Schedule[] = parser.collect_schedule(window.jQuery);
            if ( currscheds.length == 0 ) {
                console.warn("Maybe parser is failed because no collected schedule : "+conf["module"]+"."+conf["parser"]);
            }
            currscheds.forEach( (s) => {
                scheds.push(s);
            });
            basic.debug("Finished fetch '"+url+"'");
            next_collect_schedule();
        }});
    });
}

function regist_schedule(): void {
    if ( ! fs.existsSync(cachefile) ) fs.writeFileSync(cachefile, "", "utf8");
    var caches = fs.readFileSync(cachefile, "utf8").split("\n").map( (e) => {
        return e.trim();
    }).filter( (e) => {
        return e != "";
    });
    
    var entries: basic.Schedule[] = [];
    var now = Date.now();
    scheds.filter( (s) => {
        return s.start.getTime() > now;
    }).forEach( (s) => {
        var cachevalue = s.getSummary();
        if ( caches.indexOf(cachevalue) >= 0 ) {
            basic.debug("Ignore cached entry : "+s.getSummary());
            return;
        }
        entries.push(s);
        caches.push(cachevalue);
    });
    
    write_org_file(entries, () => {
        if ( dryrun ) return;
        fs.writeFileSync(cachefile, caches.join("\n")+"\n", "utf8");
    });
}

function write_org_file(entries: basic.Schedule[], success_func) {
    if ( ! fs.existsSync(orgfile) ) fs.writeFileSync(orgfile, "", "utf8");
    var proc = cp.spawn("emacs", ["-Q", "--batch", "-l", "org-entry.el", "-f", "print-org-entry", orgfile]);
    proc.on("error", (err) => {
        console.error("Failed print current org entry : "+err);
        return;
    });
    
    var buff = "";
    proc.stderr.on("data", (data) => { basic.debug(data.toString()); });
    proc.stdout.on("data", (data) => { buff += data.toString(); });
    proc.on("close", (code, sig) => {
        var olds: string[] = buff.split("\n").filter( (e) => {
            return e.match(/^###\s+/);
        }).map( (e) => {
            return e.replace(/^###\s+/, "");
        });

        var dryinfo = "";
        entries.forEach( (e) => {
            var title = e.title;
            var range = make_org_schedule_range_part(e.start, e.end);
            if ( olds.indexOf(title+" "+range) >= 0 ) {
                basic.debug("Ignore exist entry : "+e.getSummary());
                return;
            }
            if ( dryrun ) {
                dryinfo += title+" "+range+"\n";
            }
            else {
                fs.appendFileSync(orgfile, make_org_schedule(title, range, e.url), "utf8");
                console.log("Registed "+e.getSummary());
            }
        });
        
        if ( dryrun ) {
            console.log("### Here are entries will be registed ###\n"+dryinfo);
        }
        success_func();
    });
}

function make_org_schedule(title: string, range: string, url: string = ""): string {
    return "* "+title+"\n"
        +  "  "+range+"\n\n"
        +  "  :PROPERTIES:\n"
        +  "  :LOCATION: \n"
        +  "  :LINK: [["+url+"][Go to web page]]\n"
        +  "  :ID: \n"
        +  "  :END:\n\n";
}

function make_org_schedule_range_part(start: Date, end: Date): string {
    var sdt = make_org_schedule_date_part(start);
    var edt = make_org_schedule_date_part(end);
    var stm = make_org_schedule_time_part(start);
    var etm = make_org_schedule_time_part(end);
    var swd = make_org_schedule_wday_part(start);
    if ( sdt == edt ) {
        return "<"+sdt+" "+swd+" "+stm+"-"+etm+">";
    }
    else {
        var ewd = make_org_schedule_wday_part(end);
        return "<"+sdt+" "+swd+" "+stm+">--<"+edt+" "+ewd+" "+etm+">";
    }
}

function make_org_schedule_date_part(dt: Date): string {
    return dt.getFullYear()
        +  "-"+basic.padding(( dt.getMonth() + 1 ).toString(), 2)
        +  "-"+basic.padding(dt.getDate().toString(), 2);
}

function make_org_schedule_time_part(dt: Date): string {
    return basic.padding(dt.getHours().toString(), 2)
        +  ":"+basic.padding(dt.getMinutes().toString(), 2);
}

function make_org_schedule_wday_part(dt: Date): string {
    var widx = dt.getDay();
    return widx == 0 ? "日"
        :  widx == 1 ? "月"
        :  widx == 2 ? "火"
        :  widx == 3 ? "水"
        :  widx == 4 ? "木"
        :  widx == 5 ? "金"
        :  widx == 6 ? "土"
        :              "";
}

