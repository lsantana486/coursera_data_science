--select.txt
SELECT COUNT(*) FROM frequency WHERE docid='10398_txt_earn';

--select_project.txt
SELECT COUNT(term) FROM frequency WHERE docid='10398_txt_earn' and count=1;

--union.txt
SELECT COUNT(*) FROM (SELECT term FROM frequency WHERE docid='10398_txt_earn' and count=1 UNION SELECT term FROM frequency WHERE docid='925_txt_trade' and count=1) AS TMP;

--count.txt
SELECT COUNT(*) FROM frequency WHERE term = 'parliament';

--big_documents.txt
SELECT COUNT(*) FROM (SELECT docid FROM frequency GROUP BY docid HAVING SUM(count) > 300);
--SELECT COUNT(*) FROM (SELECT docid, SUM(count) as c FROM frequency GROUP BY docid) as TMP2 WHERE TMP2.c>300;

-- two_words.txt
SELECT COUNT(*) FROM (SELECT DISTINCT f1.docid FROM frequency as f1, frequency as f2 WHERE f1.term = 'transactions' AND f2.term = 'world' AND f1.docid=f2.docid GROUP BY f1.docid);
--SELECT docid, term FROM frequency GROUP BY term, docid HAVING term IN ('transactions','world') ORDER BY docid desc;