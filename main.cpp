#include <bits/stdc++.h>
using namespace std;

enum Status { ACC=0, WA=1, RE=2, TLE=3 };

static inline Status parse_status(const string &s){
    if(s=="Accepted") return ACC;
    if(s=="Wrong_Answer") return WA;
    if(s=="Runtime_Error") return RE;
    return TLE;
}

struct SubmissionRec{
    int prob; Status st; int t; // problem index 0..M-1
};

struct ProblemState{
    int wrong_revealed = 0; // wrong attempts that are already public
    bool solved = false;
    int solve_time = 0; // time of first accept (if solved)

    // Freeze session fields
    bool is_frozen = false; // has submissions after current FREEZE
    int x_at_freeze = 0; // wrong_revealed at first post-freeze submission
    vector<pair<Status,int>> frozen_subs; // submissions during current freeze
};

struct Team{
    string name;
    vector<ProblemState> prob;

    int solved_cnt = 0;
    long long penalty = 0;
    vector<int> solve_times_desc; // sorted descending

    vector<SubmissionRec> history; // for queries

    int frozen_problems_cnt = 0; // count of probs with is_frozen
};

struct SystemState{
    bool started = false;
    bool freeze_active = false;
    int M = 0; // problems count
    int T = 0; // duration (unused but parsed)

    vector<Team> teams;
    unordered_map<string,int> id_of; // name->index

    // Ranking snapshots
    vector<int> last_flushed_order; // team indices

    // live order used during SCROLL (mutates as we unfreeze)
    vector<int> cur_order;
    vector<int> pos; // team index -> position in cur_order
} st;

// Comparator for ranking
static inline bool team_better(const Team &a, const Team &b){
    if(a.solved_cnt != b.solved_cnt) return a.solved_cnt > b.solved_cnt;
    if(a.penalty != b.penalty) return a.penalty < b.penalty;
    const auto &A=a.solve_times_desc, &B=b.solve_times_desc;
    // compare lexicographically over descending arrays (smaller time better)
    size_t n=min(A.size(), B.size());
    for(size_t i=0;i<n;i++){
        if(A[i]!=B[i]) return A[i] < B[i];
    }
    if(A.size()!=B.size()) return A.size()<B.size(); // equal by count actually if solved_cnt equal; fallback safe
    return a.name < b.name;
}

static inline void insert_solve_time_desc(vector<int>& v, int t){
    // keep v sorted descending
    auto it = lower_bound(v.begin(), v.end(), t, [](int a,int b){return a>b;});
    v.insert(it, t);
}

static inline void ensure_team_probs_size(Team &tm){
    if((int)tm.prob.size()<st.M){
        tm.prob.resize(st.M);
    }
}

static void do_addteam(const string &name){
    if(st.started){
        cout << "[Error]Add failed: competition has started.\n";
        return;
    }
    if(st.id_of.count(name)){
        cout << "[Error]Add failed: duplicated team name.\n";
        return;
    }
    Team tm; tm.name=name; tm.prob.resize(st.M);
    int idx = (int)st.teams.size();
    st.teams.push_back(move(tm));
    st.id_of[name]=idx;
    cout << "[Info]Add successfully.\n";
}

static void init_last_flushed_order_lex(){
    int n=st.teams.size();
    st.last_flushed_order.resize(n);
    iota(st.last_flushed_order.begin(), st.last_flushed_order.end(), 0);
    sort(st.last_flushed_order.begin(), st.last_flushed_order.end(), [&](int i, int j){
        return st.teams[i].name < st.teams[j].name;
    });
}

static void do_start(int duration_time, int problem_count){
    if(st.started){
        cout << "[Error]Start failed: competition has started.\n";
        return;
    }
    st.started=true; st.T=duration_time; st.M=problem_count;
    for(auto &tm: st.teams) tm.prob.resize(st.M);
    cout << "[Info]Competition starts.\n";
    init_last_flushed_order_lex();
}

static inline int prob_index_from_name(const string &p){
    // p is single uppercase letter A..Z
    return (int)(p[0]-'A');
}

static void record_submit_history(Team &tm, int pi, Status stt, int time){
    tm.history.push_back({pi, stt, time});
}

static void apply_public_submission(Team &tm, int pi, Status stt, int time){
    auto &ps = tm.prob[pi];
    if(ps.solved){
        return; // already solved; submissions after don't affect public state
    }
    if(stt==ACC){
        ps.solved=true; ps.solve_time=time;
        tm.solved_cnt++;
        tm.penalty += 20LL * ps.wrong_revealed + time;
        insert_solve_time_desc(tm.solve_times_desc, time);
    }else{
        ps.wrong_revealed++;
    }
}

static void do_submit(const string &prob_name, const string &team_name, const string &status_str, int time){
    int ti = st.id_of[team_name];
    Team &tm = st.teams[ti];
    ensure_team_probs_size(tm);
    int pi = prob_index_from_name(prob_name);
    Status stt = parse_status(status_str);

    record_submit_history(tm, pi, stt, time);

    auto &ps = tm.prob[pi];
    if(st.freeze_active){
        if(ps.solved){
            // Problem solved before freeze: not frozen even if more submissions now
            return;
        }
        // buffer into frozen submissions, mark frozen lazily at first post-freeze submission
        if(!ps.is_frozen){
            ps.is_frozen=true;
            ps.x_at_freeze = ps.wrong_revealed;
            tm.frozen_problems_cnt++;
        }
        ps.frozen_subs.emplace_back(stt, time);
    }else{
        apply_public_submission(tm, pi, stt, time);
    }
}

static vector<int> make_current_order(){
    int n=st.teams.size();
    vector<int> order(n);
    iota(order.begin(), order.end(), 0);
    stable_sort(order.begin(), order.end(), [&](int i, int j){
        const Team &A=st.teams[i], &B=st.teams[j];
        if(team_better(A,B)) return true;
        if(team_better(B,A)) return false;
        return A.name < B.name;
    });
    return order;
}

static void do_flush(bool print_info=true){
    if(print_info) cout << "[Info]Flush scoreboard.\n";
    st.last_flushed_order = make_current_order();
}

static string cell_display_before_scroll(const Team &tm, const ProblemState &ps){
    if(st.freeze_active && ps.is_frozen){
        int x = ps.x_at_freeze;
        int y = (int)ps.frozen_subs.size();
        if(x==0){
            return to_string(0) + "/" + to_string(y);
        }else{
            return string("-") + to_string(x) + "/" + to_string(y);
        }
    }
    if(ps.solved){
        if(ps.wrong_revealed==0) return "+";
        return string("+") + to_string(ps.wrong_revealed);
    }else{
        if(ps.wrong_revealed==0) return ".";
        return string("-") + to_string(ps.wrong_revealed);
    }
}

static void print_scoreboard(const vector<int> &order, bool before_scroll_view){
    int n=order.size();
    for(int rank=0; rank<n; ++rank){
        const Team &tm = st.teams[ order[rank] ];
        cout << tm.name << ' ' << (rank+1) << ' ' << tm.solved_cnt << ' ' << tm.penalty;
        for(int p=0;p<st.M;p++){
            const auto &ps = tm.prob[p];
            string cell = cell_display_before_scroll(tm, ps);
            cout << ' ' << cell;
        }
        cout << "\n";
    }
}

static void move_team_up_in_order(int ti, vector<int> &order, vector<int> &pos){
    int i = pos[ti];
    while(i>0){
        int tj = order[i-1];
        if(team_better(st.teams[ti], st.teams[tj]) || (!team_better(st.teams[tj], st.teams[ti]) && st.teams[ti].name < st.teams[tj].name)){
            // swap
            order[i-1]=ti; order[i]=tj;
            pos[ti]=i-1; pos[tj]=i;
            --i;
        }else break;
    }
}

static void do_freeze(){
    if(st.freeze_active){
        cout << "[Error]Freeze failed: scoreboard has been frozen.\n";
        return;
    }
    st.freeze_active=true;
    cout << "[Info]Freeze scoreboard.\n";
}

static void do_scroll(){
    if(!st.freeze_active){
        cout << "[Error]Scroll failed: scoreboard has not been frozen.\n";
        return;
    }
    cout << "[Info]Scroll scoreboard.\n";

    // flush without printing info, and print scoreboard before scrolling
    do_flush(false);
    print_scoreboard(st.last_flushed_order, true);

    // prepare mutable order for live updates during scroll
    st.cur_order = st.last_flushed_order;
    st.pos.assign(st.teams.size(), 0);
    for(int i=0;i<(int)st.cur_order.size();++i) st.pos[ st.cur_order[i] ] = i;

    // perform unfreezing steps
    int n = st.teams.size();
    while(true){
        // find lowest-ranked team having frozen problems
        int chosen_team = -1; int chosen_pos=-1;
        for(int i=n-1;i>=0;--i){
            int ti = st.cur_order[i];
            if(st.teams[ti].frozen_problems_cnt>0){ chosen_team=ti; chosen_pos=i; break; }
        }
        if(chosen_team==-1) break; // no more frozen problems

        // choose smallest problem letter that is frozen
        int chosen_prob=-1;
        for(int p=0;p<st.M;p++){
            if(st.teams[chosen_team].prob[p].is_frozen){ chosen_prob=p; break; }
        }
        if(chosen_prob==-1){
            // should not happen; fix counter
            st.teams[chosen_team].frozen_problems_cnt = 0;
            continue;
        }
        auto &tm = st.teams[chosen_team];
        auto &ps = tm.prob[chosen_prob];

        // Apply frozen submissions to public state
        int old_pos = st.pos[chosen_team];

        for(auto &rec : ps.frozen_subs){
            if(ps.solved) break; // further subs do not change state
            if(rec.first==ACC){
                // all previous wrongs are already counted by increasing wrong_revealed along the way
                ps.solved=true; ps.solve_time=rec.second;
                tm.solved_cnt++;
                tm.penalty += 20LL * ps.wrong_revealed + rec.second;
                insert_solve_time_desc(tm.solve_times_desc, rec.second);
            }else{
                ps.wrong_revealed++;
            }
        }
        // clear frozen state for this problem
        ps.frozen_subs.clear();
        if(ps.is_frozen){
            ps.is_frozen=false;
            tm.frozen_problems_cnt--;
        }

        // update ranking position (team may move up)
        move_team_up_in_order(chosen_team, st.cur_order, st.pos);
        int new_pos = st.pos[chosen_team];
        if(new_pos < old_pos){
            // ranking changed due to this unfreeze
            string team1 = tm.name;
            string team2 = st.teams[ st.cur_order[new_pos+1] ].name; // the team previously at position new_pos before team1 moved here
            cout << team1 << ' ' << team2 << ' ' << tm.solved_cnt << ' ' << tm.penalty << "\n";
        }
    }

    // End of scrolling
    st.freeze_active=false;

    // print scoreboard after scrolling (now no frozen cells remain)
    print_scoreboard(st.cur_order, false);

    // After scrolling, update last flushed order to the final scoreboard
    st.last_flushed_order = st.cur_order;
}

static void do_query_ranking(const string &team_name){
    auto it = st.id_of.find(team_name);
    if(it==st.id_of.end()){
        cout << "[Error]Query ranking failed: cannot find the team.\n";
        return;
    }
    cout << "[Info]Complete query ranking.\n";
    if(st.freeze_active){
        cout << "[Warning]Scoreboard is frozen. The ranking may be inaccurate until it were scrolled.\n";
    }
    // find rank in last_flushed_order
    int ti = it->second; int rank=1;
    for(size_t i=0;i<st.last_flushed_order.size();++i){
        if(st.last_flushed_order[i]==ti){ rank = (int)i+1; break; }
    }
    cout << team_name << " NOW AT RANKING " << rank << "\n";
}

static void do_query_submission(const string &team_name, const string &pfilter, const string &sfilter){
    auto it = st.id_of.find(team_name);
    if(it==st.id_of.end()){
        cout << "[Error]Query submission failed: cannot find the team.\n";
        return;
    }
    cout << "[Info]Complete query submission.\n";
    Team &tm = st.teams[it->second];
    bool allp = (pfilter=="ALL");
    bool alls = (sfilter=="ALL");
    int pidx = -1; if(!allp) pidx = prob_index_from_name(pfilter);
    Status sidx = ACC; if(!alls) sidx = parse_status(sfilter);

    for(int i=(int)tm.history.size()-1;i>=0;--i){
        const auto &h = tm.history[i];
        if((allp || h.prob==pidx) && (alls || h.st==sidx)){
            char prob_char = char('A' + h.prob);
            string ststr;
            switch(h.st){
                case ACC: ststr="Accepted"; break; case WA: ststr="Wrong_Answer"; break; case RE: ststr="Runtime_Error"; break; case TLE: ststr="Time_Limit_Exceed"; break;
            }
            cout << team_name << ' ' << prob_char << ' ' << ststr << ' ' << h.t << "\n";
            return;
        }
    }
    cout << "Cannot find any submission.\n";
}

int main(){
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    string line;
    while(true){
        if(!std::getline(cin, line)) break;
        if(line.empty()) continue;
        if(line.rfind("ADDTEAM ", 0)==0){
            string name = line.substr(8);
            do_addteam(name);
        }else if(line.rfind("START DURATION ", 0)==0){
            // format: START DURATION [duration_time] PROBLEM [problem_count]
            stringstream ss(line);
            string START, DURATION, PROBLEM; int duration, pcnt;
            ss >> START >> DURATION >> duration >> PROBLEM >> pcnt;
            do_start(duration, pcnt);
        }else if(line.rfind("SUBMIT ", 0)==0){
            stringstream ss(line);
            string cmd, prob, BY, team, WITH, status, AT; int t;
            ss >> cmd >> prob >> BY >> team >> WITH >> status >> AT >> t;
            do_submit(prob, team, status, t);
        }else if(line=="FLUSH"){
            do_flush(true);
        }else if(line=="FREEZE"){
            do_freeze();
        }else if(line=="SCROLL"){
            do_scroll();
        }else if(line.rfind("QUERY_RANKING ", 0)==0){
            string name = line.substr(14);
            do_query_ranking(name);
        }else if(line.rfind("QUERY_SUBMISSION ", 0)==0){
            // QUERY_SUBMISSION [team_name] WHERE PROBLEM=[problem_name] AND STATUS=[status]
            string tmp = line.substr(17);
            stringstream ss(tmp);
            string team, WHERE, PROBLEM_eq, AND, STATUS_eq;
            ss >> team >> WHERE >> PROBLEM_eq >> AND >> STATUS_eq;
            string pval = PROBLEM_eq.substr(PROBLEM_eq.find('=')+1);
            string sval = STATUS_eq.substr(STATUS_eq.find('=')+1);
            do_query_submission(team, pval, sval);
        }else if(line=="END"){
            cout << "[Info]Competition ends.\n";
            break;
        }
    }
    return 0;
}

