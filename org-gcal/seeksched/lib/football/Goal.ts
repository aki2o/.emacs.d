/// <reference path="../basic.ts" />
/// <reference path="../football.ts" />
/// <reference path="../../typings/node/node.d.ts" />
/// <reference path="../../typings/jquery/jquery.d.ts" />

import basic = module("../basic");
import football = module("../football");

export class Goal extends football.Parser implements basic.Parser {
    collect_schedule(e: JQueryStatic): Array<Schedule> {
        return;
    }
}
