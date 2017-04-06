/// <reference path="basic.ts" />
/// <reference path="../typings/jquery/jquery.d.ts" />

import basic = require("lib/basic");

export class SportsNavi extends basic.VSSportParser {
    collect_schedule(e: JQueryStatic): basic.Schedule[] {
        var scheds: basic.VSSportSchedule[] = [];
        var entries = e("#modGameResult section .mainSection");
        for ( var i = 0; i < entries.length; i++ ) {
            var sec = entries.eq(i);
            var dt = sec.find("header").text().match(/([0-9]+)年([0-9]+)月([0-9]+)日/);
            if ( ! dt || dt.length != 4 ) {
                basic.debug("Found unexpected header : "+sec.find("header").text());
                continue;
            }
            var mtables = sec.find(".game_list");
            for ( var ii = 0; ii < mtables.length; ii++ ) {
                var trlist = mtables.eq(ii).find("tr");
                var timee: string[] = [];
                for ( var k = 0; k < trlist.length; k++ ) {
                    var tr = trlist.eq(k);
                    if ( tr.find("td").length == 1 ) {
                        timee = tr.find("td").text().match(/([0-9:]+)開始/);
                    }
                    else if ( timee.length == 2 ) {
                        var lteam = tr.find("td.BorderRightNon .inTeam1L").text().trim();
                        var rteam = tr.find("td.BorderLeftNon .inTeam1R").text().trim();
                        var vsteam = lteam == this.team ? rteam
                            :        rteam == this.team ? lteam
                            :                             null;
                        if ( ! vsteam ) {
                            basic.debug("Found non-target team : L:'"+lteam+"' R:'"+rteam+"'");
                        }
                        else {
                            var start = new Date(dt[1]+"/"+dt[2]+"/"+dt[3]+" "+timee[1]);
                            scheds.push( new basic.VSSportSchedule(start, this.team, vsteam, "2h", undefined, this.url) );
                        }
                        timee = [];
                    }
                }
            }
        }
        return scheds;
    }
}

