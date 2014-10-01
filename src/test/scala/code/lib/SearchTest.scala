import org.specs2.mutable.Specification

import code.lib._

object SearchTest extends Specification {
  "TextQuery" should {
    "parse these examples" in {
      TextQuery.parseAll(TextQuery.term, "asdf").get must beEqualTo (TermQuery("asdf"))
      TextQuery.parseAll(TextQuery.or, "asdf or zxcv").get must beEqualTo ({
        OrQuery(TermQuery("asdf"), TermQuery("zxcv"))
      })
      TextQuery.parseAll(TextQuery.and, "asdf and zxcv").get must beEqualTo ({
        AndQuery(TermQuery("asdf"), TermQuery("zxcv"))
      })
      TextQuery.parseAll(TextQuery.not, "not asdf").get must beEqualTo ({
        NotQuery(TermQuery("asdf"))
      })
      TextQuery("asdf") must beEqualTo (Some {
        TermQuery("asdf")
      })
      TextQuery("asdf and zxcv") must beEqualTo (Some {
        AndQuery(TermQuery("asdf"), TermQuery("zxcv"))
      })
      TextQuery("asdf or zxcv") must beEqualTo (Some {
        OrQuery(TermQuery("asdf"), TermQuery("zxcv"))
      })
      TextQuery("not zxcv") must beEqualTo (Some {
        NotQuery(TermQuery("zxcv"))
      })
      TextQuery("qwer and asdf and zxcv") must beEqualTo (Some {
        AndQuery(TermQuery("qwer"), AndQuery(TermQuery("asdf"), (TermQuery("zxcv"))))
      })
      TextQuery("qwer or asdf and not zxcv") must beEqualTo (Some {
        OrQuery(TermQuery("qwer"), AndQuery(TermQuery("asdf"), NotQuery(TermQuery("zxcv"))))
      })
    }
  }
}
