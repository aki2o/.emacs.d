/// <reference path="../typings/jquery/jquery.d.ts" />

var verbose: boolean;
export function setDebugOn(): void {
    verbose = true;
}

export function debug(s: string) {
    if ( ! verbose ) return;
    console.log(s);
}

export function padding(s: string, digit: number, padchar: string = "0"): string {
    var padstr = "";
    for ( var i = 0; i < digit; i++ ) {
        padstr += padchar;
    }
    return ( padstr + s ).slice(0 - digit);
}

export class Schedule {
    constructor(public start: Date,
                public end: Date,
                public title: string,
                public url: string = "") {
    }
    toString(): string {
        return this.start.toString() + " - " + this.end.toString() + "\n"
            +  this.title + "\n"
            +  this.url + "\n\n";
    }
    getSummary(): string {
        return this.start.toString() + " " + this.title;
    }
}

export interface Parser {
    collect_schedule(e: JQueryStatic): Schedule[];
}


/////////////////
// For VSSport

export class VSSportSchedule extends Schedule {
    
    constructor(public start: Date,
                private myteam: string,
                private vsteam: string,
                private matchtime: string,
                private matchinfo: string = "Unknown",
                public url: string = "") {
        var end = new Date(start.toString());
        var timee = matchtime.match(/^([0-9]+)([a-z])$/);
        if ( timee[2] == "h" ) {
            end.setHours(start.getHours() + Number(timee[1]));
        }
        super(start, end, myteam+" x "+vsteam+" ("+matchinfo+")", url);
    }

    getSummary(): string {
        var e:string[] = [
            this.myteam.replace(/\s+/g, "").toLowerCase(),
            this.vsteam.replace(/\s+/g, "").toLowerCase()
        ];
        var ee = e.sort();
        var dt = this.start.getFullYear()
            +    "/"+padding(( this.start.getMonth() + 1 ).toString(), 2)
            +    "/"+padding(this.start.getDate().toString(), 2);
        return dt+" "+ee[0]+" x "+ee[1];
    }
    
}

export class VSSportParser implements Parser {

    public team: string;
    public url: string;
    
    constructor(private conf: Object) {
        this.team = conf["team"];
        this.url = conf["url"];
    }

    get_flat_text(s: string): string {
        return s.replace(/\s+/g, " ");
    }

    collect_schedule(e: JQueryStatic): Schedule[] {
        var scheds: Schedule[] = [];
        return scheds;
    }
    
}

