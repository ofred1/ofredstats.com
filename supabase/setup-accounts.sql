-- Run once in Supabase > SQL Editor. No secret keys are required.
begin;
create table if not exists public.laymaker_members (
 user_id uuid primary key references auth.users(id) on delete cascade,
 approved boolean not null default false,
 created_at timestamptz not null default now()
);
create table if not exists public.laymaker_boards (
 id text primary key check(id='current'),
 payload jsonb not null check(jsonb_typeof(payload)='object'),
 updated_at timestamptz not null default now()
);
alter table public.laymaker_members enable row level security;
alter table public.laymaker_boards enable row level security;
revoke all on public.laymaker_members, public.laymaker_boards from anon, authenticated;
grant select on public.laymaker_members, public.laymaker_boards to authenticated;
drop policy if exists "Read own access" on public.laymaker_members;
create policy "Read own access" on public.laymaker_members for select to authenticated using (user_id=(select auth.uid()));
drop policy if exists "Approved members read board" on public.laymaker_boards;
create policy "Approved members read board" on public.laymaker_boards for select to authenticated using (
 exists(select 1 from public.laymaker_members where user_id=(select auth.uid()) and approved=true)
);
insert into public.laymaker_boards(id,payload) values ('current',$board${"date":"2026-11-05","picks":[],"hot_take":null,"hot_take_id":null,"daily_pick_id":null,"must_picks":[],"must_pick_ids":[],"ofred_pick_ids":[],"suggested_parlays":[],"ofred_parlays":[],"history":{"picks":[],"suggested_parlays":[],"ofred_picks":[],"ofred_parlays":[],"hot_takes":[],"daily_picks":[]},"archived_through":"2026-11-04","archived_parlay_outcomes":[{"legs":2,"wins":1,"losses":0,"pushes":0,"voids":0}],"archived_daily_streak":{"type":"win","count":3},"archived_records":{"locks":{"wins":0,"losses":0,"pushes":0,"voids":0},"likely":{"wins":5,"losses":0,"pushes":0,"voids":0},"hot":{"wins":3,"losses":0,"pushes":0,"voids":0},"daily":{"wins":3,"losses":0,"pushes":0,"voids":0},"suggested":{"wins":1,"losses":0,"pushes":0,"voids":0},"ofred":{"wins":1,"losses":0,"pushes":0,"voids":0}}}$board$::jsonb)
on conflict(id) do nothing;
commit;
-- To approve a registered friend, run the following separately, replacing their email:
-- insert into public.laymaker_members(user_id,approved)
-- select id,true from auth.users where lower(email)=lower('FRIEND_EMAIL_HERE')
-- on conflict(user_id) do update set approved=excluded.approved;
-- To revoke: update public.laymaker_members set approved=false where user_id='USER_UUID';
