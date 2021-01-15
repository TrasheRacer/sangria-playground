import models.{CharacterRepo, SchemaDefinition}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsObject, JsString, Json}
import sangria.ast.Document
import sangria.execution.Executor
import sangria.execution.deferred.DeferredResolver
import sangria.macros._
import sangria.marshalling.playJson._
import sangria.marshalling.{CoercedScalaResultMarshaller, FromInput, ResultMarshaller}
import sangria.schema.{Argument, Field, InputField, InputObjectType, LongType, ObjectType, Schema, fields}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Below are simplified versions of the classes, objects and traits used in the real implementation of our schema,
  * to reproduce the bug with minimal complexity.
  */
object SchemaInitTest {
  case class ProjectData(name: String = "empty_project", activities: Seq[ActivityRef] = Seq.empty)
  case class Project(projectData: ProjectData)
  case class QueryableProject(project: Project)
  object QueryableProject {
    val empty: QueryableProject = QueryableProject(Project(ProjectData()))
  }
  trait ProjectsContext[T <: ProjectsContext[T]]
  trait POWContext extends ProjectsContext[POWContext]
  case class ProjectValue[T](project: QueryableProject, value: T)
  object ProjectValue {
    def ofActivityRef(queryableProject: QueryableProject, activityRef: ActivityRef): ProjectValue[ActivityRef] =
      ProjectValue(queryableProject, activityRef)
  }

  case class ActivityRef(id: Long)
  object ActivityRef {
    implicit val fromInput: FromInput[ActivityRef] = new FromInput[ActivityRef] {
      override val marshaller: ResultMarshaller = CoercedScalaResultMarshaller.default
      override def fromResult(node: marshaller.Node): ActivityRef = ActivityRef(3456L)
    }

    // TODO: FIND OUT WHY OUR BUG DISAPPEARS WHEN THIS IS CHANGED INTO A LAZY VAL
    val arg: Argument[ActivityRef] = Argument("activityRef", inputType)
  }

  val inputType: InputObjectType[ActivityRef] =
    InputObjectType[ActivityRef](
      "ActivityRefInput",
      fields = List(
        InputField("id", LongType)
      )
    )

  // TODO: FIND OUT WHY NPE DISAPPEARS WHEN THIS IS MADE LAZY
  val activityRefArgs = ActivityRef.arg :: Nil
}

class SchemaSpec extends AnyWordSpec with Matchers {
  "StartWars Schema" should {
    "correctly identify R2-D2 as the hero of the Star Wars Saga" in {
      val query =
        graphql"""
         query HeroNameQuery {
           hero {
             name
           }
         }
       """

      executeQuery(query) should be (Json.parse(
        """
         {
           "data": {
             "hero": {
               "name": "R2-D2"
             }
           }
         }
        """))
    }

    "allow to fetch Han Solo using his ID provided through variables" in {
      val query =
        graphql"""
         query FetchSomeIDQuery($$humanId: String!) {
           human(id: $$humanId) {
             name
             friends {
               id
               name
             }
           }
         }
       """

      executeQuery(query, vars = Json.obj("humanId" → JsString("1002"))) should be (Json.parse(
        """
         {
           "data": {
             "human": {
               "name": "Han Solo",
               "friends": [
                 {
                   "id": "1000",
                   "name": "Luke Skywalker"
                 },
                 {
                   "id": "1003",
                   "name": "Leia Organa"
                 },
                 {
                   "id": "2001",
                   "name": "R2-D2"
                 }
               ]
             }
           }
         }
        """))
    }

    "ObjectType() and Schema() should not have any kind of weird side effects when sharing an argument" in {
      import SchemaInitTest._

      // Do something which (apparently) mutates global state somewhere in sangria
      ObjectType(
        "Activity",
        fields[Unit, QueryableProject](
          Field(
            "activityRef",
            LongType,
            arguments = ActivityRef.arg :: Nil,
            resolve = _ => 123L // The resolve function is irrelevant to this test
          )
        )
      )

      val query =       ObjectType(
        name = "Query",
        fields = fields[POWContext, Unit](
          Field(
            name = "project",
            fieldType = ObjectType[Unit, QueryableProject](
              name = "Project",
              fields = fields[Unit, QueryableProject](
                Field(
                  "activityRef",
                  LongType,
                  arguments = activityRefArgs,
                  resolve = _ => 123L // The resolve function is irrelevant to this test
                )
              )
            ),
            resolve = _ => QueryableProject.empty // The resolve function is irrelevant to this test
          )
        )
      )


      // Create a Schema which **shouldn't** access any global mutable state
      Schema(query)

      // No exception should be thrown...
      assert(true)
    }
  }

  def executeQuery(query: Document, vars: JsObject = Json.obj()) = {
    val futureResult = Executor.execute(SchemaDefinition.StarWarsSchema, query,
      variables = vars,
      userContext = new CharacterRepo,
      deferredResolver = DeferredResolver.fetchers(SchemaDefinition.characters))

    Await.result(futureResult, 10.seconds)
  }
}
