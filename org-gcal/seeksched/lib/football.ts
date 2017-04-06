/// <reference path="basic.ts" />
/// <reference path="../typings/jquery/jquery.d.ts" />

import basic = require("lib/basic");

export class Goal extends basic.VSSportParser {
    collect_schedule(e: JQueryStatic): basic.Schedule[] {
        var scheds: basic.VSSportSchedule[] = [];
        var mtables = e("table.match-table");
        for ( var i = 0; i < mtables.length; i++ ) {
            var t = mtables.eq(i);
            var teams = t.find("td.team");
            var is_target = false;
            var vsteam: string;
            for ( var ii = 0; ii < teams.length; ii++ ) {
                var teamnm = teams.eq(ii).text().trim();
                if ( teamnm == this.team ) {
                    is_target = true;
                }
                else {
                    vsteam = teamnm;
                }
            }
            var dt = t.find(".comp-date").text().trim().match(/^([0-9]+)年([0-9]+)月([0-9]+)日/);
            var starttime = t.find("td.status").text().trim();
            if ( ! is_target || dt.length != 4 || ! starttime.match(/^[0-9]+:[0-9]+$/) ) {
                basic.debug("Found unexpected entry : "+this.get_flat_text(t.text()));
                continue;
            }
            var start = new Date(dt[1]+"/"+dt[2]+"/"+dt[3]+" "+starttime);
            scheds.push( new basic.VSSportSchedule(start, this.team, vsteam, "2h", "Unknown", this.url) );
        }
        return scheds;
    }
}

export class Google extends basic.VSSportParser {
    collect_schedule(e: JQueryStatic): basic.Schedule[] {
        var scheds: basic.VSSportSchedule[] = [];
        var entries = e("#lr_tab_unit_uid_1 tr._Fb._jZc");
        for ( var i = 0; i < entries.length; i++ ) {
            var tr = entries.eq(i);
            var le = tr.find("td.vk_pl > div").children("div");
            var re = tr.find("td.vk_pr > div").children("div");
            if ( le.length < 2 || re.length < 2 ) {
                basic.debug("Found unexpected cell : L:'"+this.get_flat_text(le.text())+"' R:'"+this.get_flat_text(re.text())+"'");
                continue;
            }
            var info1 = le.eq(0).text();
            var info2 = re.eq(0).text();
            var minfo1 = info1.match(/^\s*([0-9\/]+)\s+-\s+(.+)\s*$/);
            var team1 = le.eq(1).text().trim();
            var minfo2 = info2.match(/^\s*([0-9:]+)/);
            var team2 = re.eq(1).text().trim();
            var vsteam = team1 == this.team ? team2
                :        team2 == this.team ? team1
                :                             undefined;
            if ( ! vsteam ) {
                basic.debug("Found unknown team : L:'"+this.get_flat_text(team1)+"' R:'"+this.get_flat_text(team2)+"'");
                continue;
            }
            if ( minfo1.length < 2 || minfo2.length < 1 ) {
                basic.debug("Found unexpected matchinfo : L:'"+this.get_flat_text(info1)+"' R:'"+this.get_flat_text(info2)+"'");
                continue;
            }
            var start = new Date(minfo1[1]+" "+minfo2[1]);
            scheds.push( new basic.VSSportSchedule(start, this.team, vsteam, "2h", minfo1[2], this.url) );
        }
        return scheds;
    }
}

export class SanSpo extends basic.VSSportParser {
    collect_schedule(e: JQueryStatic): basic.Schedule[] {
        var scheds: basic.VSSportSchedule[] = [];
        var yearinfo = e("#scmsMainContentsSection h3.ContentsTitle03");
        var yeare = yearinfo.text().match(/([0-9]+)年/);
        if ( ! yeare || yeare.length != 2 ) {
            basic.debug("Found unexpected year part : "+this.get_flat_text(yearinfo.text()));
            return scheds;
        }
        var entries = e("#scmsMainContentsSection tr");
        for ( var i = 0; i < entries.length; i++ ) {
            var tr = entries.eq(i);
            var cols = tr.find("td");
            if ( cols.length < 8 ) {
                basic.debug("Found unexpected table row : "+this.get_flat_text(tr.text()));
                continue;
            }
            var sdpart = cols.eq(0).text().trim();
            var stpart = cols.eq(3).text().trim();
            if ( ! sdpart.match(/^[0-9]+\/[0-9]+$/) || ! stpart.match(/^[0-9]+:[0-9]+$/) ) {
                basic.debug("Found unexpected table row : "+this.get_flat_text(tr.text()));
                continue;
            }
            var start = new Date(yeare[1]+"/"+sdpart+" "+stpart);
            var minfo = cols.eq(5).text().trim();
            var vsteam = cols.eq(6).text().trim();
            scheds.push( new basic.VSSportSchedule(start, this.team, vsteam, "2h", minfo, this.url) );
        }
        return scheds;
    }
}

